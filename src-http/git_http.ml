(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let (>>=) = Lwt.bind

module Log = struct
  let src = Logs.Src.create "git.http" ~doc:"logs git's http events"
  include (val Logs.src_log src : Logs.LOG)
end

module type CLIENT = sig
  module IO : Cohttp_lwt_s.IO with type 'a t = 'a Lwt.t

  val close_out: IO.oc -> unit                 (* FIXME in cohttp *)
  val close_in: IO.ic -> unit                  (* FIXME in cohttp *)
end

module type CHAN = sig
  type t
  val make:
    ?close:(unit -> unit Lwt.t) -> (Cstruct.t -> int -> int -> int Lwt.t) -> t
end

module Flow(HTTP: CLIENT) (IC: CHAN) (OC: CHAN) = struct
  open Cohttp

  module Request = struct
    include Request
    include (Request.Make(HTTP.IO)
             : Cohttp.S.Http_io with type t := t and module IO = HTTP.IO)
  end
  module Response = struct
    include Response
    include (Response.Make(HTTP.IO)
             : Cohttp.S.Http_io with type t := t and module IO = HTTP.IO)
  end

  let write_footer oc = function
    | Transfer.Chunked -> HTTP.IO.write oc "0\r\n\r\n"
    | Transfer.Fixed _
    | Transfer.Unknown -> Lwt.return_unit

  let make_oc oc =
    OC.make
      ~close:(fun () ->
          Log.debug (fun l -> l "Closing outgoing connection.");
          HTTP.close_out oc;
          Lwt.return_unit)
      (fun buf off len ->
         let str = Bytes.create len in
         Cstruct.blit_to_bytes buf off str 0 len;
         HTTP.IO.write oc (Bytes.to_string str) >>= fun () ->
         Lwt.return len)

  exception Redirect of Uri.t

  type 'a http_callback =
      Uri.t -> (HTTP.IO.ic * HTTP.IO.oc -> 'a Lwt.t) -> 'a Lwt.t

  type 'a callback = Uri.t -> (IC.t * OC.t -> 'a Lwt.t) -> 'a Lwt.t

  type context = {
    mutable state     : [`Read | `Write];
    mutable out_stream: string list;
    mutable in_stream : string list;
    mutable ic        : HTTP.IO.ic;
    mutable oc        : HTTP.IO.oc;
    mutable reader    : Response.reader option;
    mutable last_chunk: string option;
  }

  let context ic oc = {
    state = `Write; ic; oc;
    out_stream = []; in_stream = [];
    reader = None; last_chunk = None;
  }

  (* Transform the raw outgoing channel [oc] into an higher-level
     outgoing channel over HTTP. *)
  let http_oc ctx reconnect req =
    match Cohttp.Request.meth req with
    | `POST ->
      OC.make
        ~close:(fun () ->
            Log.debug (fun l -> l "Closing outgoing connection");
            HTTP.close_out ctx.oc;
            Lwt.return_unit)
        (fun buf off len ->
           begin
             if not (ctx.state = `Read) then Lwt.return_unit
             else (
               Log.debug (fun l -> l "Write need reconnects.");
               reconnect ctx
             ) end >>= fun () ->
           let chunk = Bytes.create len in
           Cstruct.blit_to_string buf off chunk 0 len;
           ctx.out_stream <- ctx.out_stream @ [Bytes.to_string chunk];
           let writer =
             Request.make_body_writer ~flush:true req ctx.oc
           in
           Request.write_body writer (Bytes.to_string chunk) >>= fun () ->
           Lwt.return len
        )
    | _  -> make_oc ctx.oc


  (* FIXME Should be moved into cohttp *)
  let check_redirect r =
    let status = Cohttp.Response.status r in
    let status_code = Cohttp.Code.code_of_status status in
    let status = Cohttp.Code.string_of_status status in
    if Cohttp.Code.is_redirection status_code then (
      let uri =
        try
          Cohttp.Response.headers r
          |> Cohttp.Header.to_list
          |> List.assoc "location"
          |> Uri.of_string
        with Not_found ->
          failwith status
      in
      Lwt.fail (Redirect uri)
    ) else
      Lwt.return_unit

  let on_success r fn =
    let status = Cohttp.Response.status r in
    let status_code = Cohttp.Code.code_of_status status in
    if Cohttp.Code.is_success status_code then fn () else (
      let status = Cohttp.Code.string_of_status status in
      Log.err (fun l -> l "with_http: %s" status);
      failwith status
    )

  let flush_oc ctx req =
    if ctx.state = `Write then (
      Log.debug (fun l -> l "Read flushes the outgoing connection");
      write_footer ctx.oc (Request.encoding req) >>= fun () ->
      ctx.state <- `Read;
      Lwt.return_unit
    ) else (
      Log.debug (fun l -> l "Read uses the existing connection");
      Lwt.return_unit
    )

  let read_exactly read n =
    let res = Cstruct.create n in
    let rec aux off =
      if off >= n then Lwt.return_unit
      else (
        read res off (n-off) >>= function
        | 0 -> Lwt.fail (Failure "read_exactly")
        | i -> aux (off + i))
    in
    aux 0 >>= fun () ->
    Lwt.return res

  let replay_in_stream read in_stream =
    Log.debug (fun l -> l "Replay the incoming connection history");
    Lwt_list.iter_s (fun s ->
        read_exactly read (String.length s) >>= fun s' ->
        if s <> Cstruct.to_string s' then
          Lwt.fail @@
          Failure (
            Printf.sprintf "replay_in_stream: expected %s, got %s."
              s (Cstruct.to_string s'))
        else Lwt.return_unit
      ) in_stream
    >>= fun () ->
    Log.debug (fun l -> l "Replay complete!");
    Lwt.return_unit

  let replay_out_stream req oc out_stream =
    Log.debug (fun l -> l "Replay the outgoing connection history");
    Request.write_header req oc >>= fun () ->
    let writer = Request.make_body_writer ~flush:false req oc in
    Lwt_list.iter_s (Request.write_body writer) out_stream >>= fun () ->
    Log.debug (fun l -> l "Replay complete!");
    Lwt.return_unit

  (* Transform the raw incoming channel [ic] into an higher-level
     incoming channel over HTTP. *)
  let http_ic ctx req =
    let read reader buf off len =
      let len = min len (Cstruct.len buf - off) in
      let read_in_chunk chunk =            (* Use [chunk] as read buffer. *)
        let blit len =
          Cstruct.blit_from_string chunk 0 buf off len;
          Lwt.return len
        in
        let n = String.length chunk in
        if n <= len then (
          ctx.last_chunk <- None;
          blit n;
        ) else (
          let tl = String.sub chunk len (n - len) in
          ctx.last_chunk <- Some tl;
          blit len
        )
      in
      match ctx.last_chunk with
      | Some c -> read_in_chunk c
      | None ->
        Response.read_body_chunk reader >>= function
        | Cohttp.Transfer.Done -> Lwt.return 0
        | Cohttp.Transfer.Chunk chunk -> read_in_chunk chunk
        | Cohttp.Transfer.Final_chunk chunk -> read_in_chunk chunk
    in
    let read_and_save reader buf off len =
      read reader buf off len >>= fun len ->
      let chunk = Cstruct.copy buf off len in
      ctx.in_stream <- ctx.in_stream @ [chunk];
      Lwt.return len
    in
    IC.make
      ~close:(fun () ->
          Log.debug (fun l -> l "Closing input connection");
          HTTP.close_in ctx.ic;
          Lwt.return_unit)
      (fun bytes off len -> match ctx.reader with
         | Some reader -> read_and_save reader bytes off len
         | None ->
           flush_oc ctx req >>= fun () ->
           Response.read ctx.ic >>= function
           | `Ok r ->
             check_redirect r >>= fun () ->
             on_success r (fun () ->
                 let r = Response.make_body_reader r ctx.ic in
                 ctx.reader     <- Some r;
                 ctx.last_chunk <- None;
                 replay_in_stream (read r) ctx.in_stream >>= fun () ->
                 read_and_save r bytes off len)
           | `Eof       -> Lwt.return 0
           | `Invalid i -> Lwt.fail (Failure i))

  let mk_headers = function
    | None   -> Cohttp.Header.init ()
    | Some h -> h

  let mk_request uri meth headers =
    let meth = (meth :> Cohttp.Code.meth) in
    match meth with
    | `POST -> Cohttp.Request.make_for_client ~headers ~chunked:true  meth uri
    | _     -> Cohttp.Request.make_for_client ~headers ~chunked:false meth uri

  exception OK
  let () =
    let fn = !Lwt.async_exception_hook in
    Lwt.async_exception_hook := (function
        | OK  -> ()
        | exn -> fn exn
      )

  let ignore_exn f x =
    Lwt.catch
      (fun () -> try f x; Lwt.return_unit with e -> Lwt.fail e)
      (function e ->
        (* WARNING: do not catch `Unix` exception here, as it will
           bring a unwanted dependency to unix.cma *)
        Log.debug (fun l -> l "Ignoring exn: %s" (Printexc.to_string e));
        Lwt.return_unit)

  (* The smart HTTP protocols simulates a flow using the following "trick":

     - every time the client wants to stream some data to the client,
       it opens a new connection and replay all the previous history
       of the stream.

     - the server is stateless and react in a deterministic way to the
       client stream. It only replies to the last "question" of the
       client stream.

  *)
  let rec http_call ?headers ?(redirects=0) meth with_conduit uri fn =
    let headers = mk_headers headers in
    let request = mk_request uri meth headers in
    let wait, wakeup = Lwt.task () in
    let wait = wait >>= fun _ -> Lwt.fail OK in
    let connect () =
      let t, u = Lwt.task () in
      Lwt.ignore_result @@ with_conduit uri (fun (nic, noc) ->
          Lwt.wakeup u (context nic noc);
          wait
        );
      t
    in
    let reconnect ctx =
      let t, u = Lwt.task () in
      ignore_exn HTTP.close_in ctx.ic  >>= fun () ->
      ignore_exn HTTP.close_out ctx.oc >>= fun () ->
      ctx.reader <- None;
      ctx.last_chunk <- None;
      Lwt.ignore_result @@ with_conduit uri (fun (nic, noc) ->
          ctx.ic <- nic;
          ctx.oc <- noc;
          ctx.state <- `Write;
          replay_out_stream request noc ctx.out_stream >>= fun () ->
          Lwt.wakeup u ();
          wait
        );
      t
    in
    let process http_ic http_oc () =
      Lwt.catch
        (fun () -> fn (http_ic, http_oc))
        (function
          | Redirect uri ->
            let redirects = redirects + 1 in
            if redirects > 10 then Lwt.fail (Failure "Too many redirects")
            else http_call ~headers ~redirects meth with_conduit uri fn
          | e -> Lwt.fail e)
    in
    connect () >>= fun ctx ->
    let http_oc = http_oc ctx reconnect request in
    let http_ic = http_ic ctx request in
    Request.write_header request ctx.oc >>= fun () ->
    Lwt.finalize
      (process http_ic http_oc)
      (fun () -> Lwt.wakeup wakeup (); Lwt.return_unit)

  let with_http ?init with_conduit uri fn =
    Log.debug (fun l -> l "HTTP connecting to %s" (Uri.to_string uri));
    let headers = match init with
      | None -> Cohttp.Header.of_list []
      | Some s ->
        (* FIXME: sorry *)
        let l = Marshal.from_string s 0 in
        Cohttp.Header.of_list l
    in
    Log.debug (fun l ->
        l "HTTP headers: %s"
          (Sexplib.Sexp.to_string (Cohttp.Header.sexp_of_t headers)));
    let meth =
      let path = Uri.path uri in
      let info = Filename.basename (Filename.dirname path) in
      let refs = Filename.basename path in
      match info, refs with
      | "info", "refs" -> `GET
      | _ -> `POST
    in
    http_call ~headers meth with_conduit uri fn

end
