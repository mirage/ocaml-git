(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module type CLIENT = sig
  include Cohttp_lwt.Client
    with type 'a IO.t = 'a Lwt.t               (* FIMXE in cohttp *)
     and type 'a Request.IO.t = 'a Lwt.t       (* FIXME in cohttp *)
     and type 'a Response.IO.t = 'a Lwt.t      (* FIXME in cohttp *)
  val close_out: IO.oc -> unit                 (* FIXME in cohttp *)
  val close_in: IO.ic -> unit                  (* FIXME in cohttp *)
  val oc: IO.oc -> Request.IO.oc               (* FIXME in cohttp *)
  val ic: IO.ic -> Response.IO.ic              (* FIXME in cohttp *)
end

module type CHAN = sig
  type t
  val make:
    ?close:(unit -> unit Lwt.t) -> (Cstruct.t -> int -> int -> int Lwt.t) -> t
end

module Flow(HTTP: CLIENT) (IC: CHAN) (OC: CHAN) = struct

  let make_oc oc =
    OC.make
      ~close:(fun () ->
          HTTP.close_out oc;
          Lwt.return_unit)
      (fun buf off len ->
         let str = Bytes.create len in
         Cstruct.blit_to_string buf off str 0 len;
         HTTP.IO.write oc str >>= fun () ->
         Lwt.return len)

  exception Redirect of Uri.t

  type 'a http_callback =
      Uri.t -> (HTTP.IO.ic * HTTP.IO.oc -> 'a Lwt.t) -> 'a Lwt.t

  type 'a callback = Uri.t -> (IC.t * OC.t -> 'a Lwt.t) -> 'a Lwt.t

  (* Abstract [fn] (which works on unchunked channels) to work on
     chunked channels:
     - POST requests are automatically chunked
     - chunked responses are automtically unchunked
  *)
  let rec http_call ?headers ?(redirects=0) meth with_conduit uri fn =
    let headers =
      match headers with None -> Cohttp.Header.init () | Some h -> h
    in
    let callback (ic, oc) =
      let req = match meth with
        | `POST ->
          Cohttp.Request.make_for_client ~headers ~chunked:true
            (meth :> Cohttp.Code.meth) uri
        | _ ->
          Cohttp.Request.make_for_client ~headers ~chunked:false
            (meth :> Cohttp.Code.meth) uri
      in
      let http_oc =
        match meth with
        | `POST ->
          let writer =
            HTTP.Request.make_body_writer ~flush:true req (HTTP.oc oc)
          in
          OC.make
            ~close:(fun () -> HTTP.close_out oc; Lwt.return_unit)
            (fun buf off len ->
               let chunk = Bytes.create len in
               Cstruct.blit_to_string buf off chunk 0 len;
               HTTP.Request.write_body writer chunk >>= fun () ->
               Lwt.return len)
        | _  -> make_oc oc
      in
      let flush_http_oc () =
        Log.debug "Closing output connection";
        HTTP.Request.write_footer req (HTTP.oc oc)
      in
      let http_ic =
        let reader = ref None in
        let old_chunk = ref None in
        let read reader buf off len =
          let read_in_chunk chunk =            (* Use [chunk] as read buffer. *)
            let blit len =
              Cstruct.blit_from_string chunk 0 buf off len;
              Lwt.return len
            in
            let n = String.length chunk in
            if n <= len then (
              old_chunk := None;
              blit n;
            ) else (
              let tl = String.sub chunk len (n - len) in
              old_chunk := Some tl;
              blit len
            )
          in
          match !old_chunk with
          | Some c -> read_in_chunk c
          | None ->
            HTTP.Response.read_body_chunk reader >>= function
            | Cohttp.Transfer.Done -> Lwt.return 0
            | Cohttp.Transfer.Chunk chunk -> read_in_chunk chunk
            | Cohttp.Transfer.Final_chunk chunk -> read_in_chunk chunk
        in
        IC.make
          ~close:(fun () -> HTTP.close_in ic; Lwt.return_unit)
          (fun bytes off len ->
             let ic = HTTP.ic ic in
             match !reader with
             | None ->
               begin
                 flush_http_oc () >>= fun () ->
                 HTTP.Response.read ic >>= function
                 | `Ok r ->
                   let status = HTTP.Response.status r in
                   let status_code = Cohttp.Code.code_of_status status in
                   let status = Cohttp.Code.string_of_status status in
                   if Cohttp.Code.is_redirection status_code then (
                     let uri =
                       try
                         HTTP.Response.headers r
                         |> Cohttp.Header.to_list
                         |> List.assoc "location"
                         |> Uri.of_string
                       with Not_found ->
                         failwith status
                     in
                     Lwt.fail (Redirect uri)
                   ) else if Cohttp.Code.is_success status_code then (
                     let r = HTTP.Response.make_body_reader r ic in
                     reader := Some r;
                     Lwt.return_unit
                   ) else (
                     Log.error "with_http: %s" status;
                     failwith status
                   )
                 | `Eof       -> Lwt.return_unit
                 | `Invalid i -> Lwt.fail (Failure i)
               end >>= fun () ->
               begin match !reader with
                 | Some reader -> read reader bytes off len
                 | None        -> Lwt.return 0
               end
             | Some reader -> read reader bytes off len)
      in
      HTTP.Request.write_header req (HTTP.oc oc) >>= fun () ->
      Lwt.catch
        (fun () -> fn (http_ic, http_oc))
        (function
          | Redirect uri ->
            let redirects = redirects + 1 in
            if redirects > 10 then Lwt.fail (Failure "Too many redirects")
            else http_call ~headers ~redirects meth with_conduit uri fn
          | e -> Lwt.fail e)
    in
    with_conduit uri callback

    let with_http ?init with_conduit uri fn =
    Log.debug "HTTP connecting to %s" (Uri.to_string uri);
    let headers = match init with
      | None -> Cohttp.Header.of_list []
      | Some s ->
        (* FIXME: sorry *)
        let l = Marshal.from_string s 0 in
        Cohttp.Header.of_list l
    in
    Log.debug "HTTP headers: %s"
      (Sexplib.Sexp.to_string (Cohttp.Header.sexp_of_t headers));
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
