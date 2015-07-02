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

open Lwt.Infix

module Log = Log.Make(struct let section = "mirage" end)

module type FS = sig
  include V1_LWT.FS with type page_aligned_buffer = Cstruct.t
  val connect: unit -> [`Error of error | `Ok of t ] Lwt.t
  val string_of_error: error -> string
end

module FS (FS: FS) = struct

  let (>>|) x f =
    x >>= function
    | `Ok x    -> f x
    | `Error e ->
      let str = FS.string_of_error e in
      Log.error "%s" str;
      Lwt.fail (Failure str)

  module M = struct

    let file_exists t f =
      Log.debug "file_exists %s" f;
      FS.stat t f >>= function
      | `Ok _    -> Lwt.return true
      | `Error _ -> Lwt.return false

    let is_directory t dir =
      Log.debug "is_directory %s" dir;
      FS.stat t dir >>| fun s ->
      Lwt.return s.FS.directory

    let parent_dir = function
      | "/"
      | "." -> None
      | s   -> Some (Filename.dirname s)

    let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return_unit)

    let mkdir t dirname =
      Log.debug "mkdir %s" dirname;
      let rec aux dir =
        file_exists t dir >>= function
        | true  -> Lwt.return_unit
        | false ->
          match parent_dir dir with
          | None   -> Lwt.return_unit
          | Some d ->
            aux d >>= fun () ->
            FS.mkdir t dir >>| fun () ->
            Lwt.return_unit
      in
      Lwt_pool.use mkdir_pool (fun () -> aux dirname)

    let list_files t kind dir =
      Log.debug "list_files %s" dir;
      file_exists t dir >>= function
      | true ->
        FS.listdir t dir >>| fun l ->
        let l = List.filter (fun s -> s <> "." && s <> "..") l in
        let l = List.map (Filename.concat dir) l in
        Lwt_list.filter_s kind l
      | false ->
        Lwt.return_nil

    let directories t dir =
      Log.debug "directories %s" dir;
      list_files t (fun f -> Lwt.catch
                       (fun () -> is_directory t f)
                       (fun _ -> Lwt.return false)
                   ) dir

    let files t dir =
      Log.debug "files %s" dir;
      list_files t (fun f -> Lwt.catch
                       (fun () -> is_directory t f >>= fun b -> Lwt.return (not b))
                       (fun _ -> Lwt.return false)
                   ) dir

    let rec remove t dir =
      Log.debug "remove %s" dir;
      let destroy dir =
        FS.destroy t dir >>| fun () ->
        Lwt.return_unit in
      files t dir                   >>= fun ls ->
      Lwt_list.iter_s destroy ls    >>= fun () ->
      directories t dir             >>= fun ds ->
      Lwt_list.iter_s (remove t) ds >>= fun () ->
      destroy dir

    let rec_files t dir =
      Log.debug "rec_files %s" dir;
      let rec aux accu dir =
        directories t dir >>= fun ds ->
        files t dir       >>= fun fs ->
        Lwt_list.fold_left_s aux (fs @ accu) ds in
      aux [] dir

    let read_file t file =
      Log.debug "read_file %s" file;
      FS.stat t file >>| fun s ->
      is_directory t file >>= function
      | false ->
        FS.read t file 0 (Int64.to_int s.FS.size) >>| fun bs ->
        let s = Cstruct.copyv bs in
        Lwt.return (Cstruct.of_string s)
      | true -> Lwt.fail (Failure (Printf.sprintf "%s is a directory" file))

    let write_file t ?temp_dir:_ file b =
      Log.debug "write_file %s" file;
      mkdir t (Filename.dirname file) >>= fun () ->
      FS.create t file    >>| fun () ->
      FS.write t file 0 b >>| fun () ->
      Lwt.return_unit

    let getcwd () =
      Lwt.return "/"

    let realpath file =
      Lwt.return file

    let stat_info _file =
      failwith "TODO"

    let chmod _t _file _perm =
      Lwt.return_unit

    let connect fn =
      FS.connect () >>| fn

    let mkdir dir =
      connect (fun t -> mkdir t dir)

    let remove file =
      connect (fun t -> remove t file)

    let file_exists file =
      connect (fun t -> file_exists t file)

    let directories dir =
      connect (fun t -> directories t dir)

    let files dir =
      connect (fun t -> files t dir)

    let rec_files dir =
      connect (fun t -> rec_files t dir)

    let read_file file =
      connect (fun t -> read_file t file)

    let write_file file ?temp_dir buf =
      connect (fun t -> write_file t ?temp_dir file buf)

    let chmod file perm =
      connect (fun t -> chmod t file perm)

  end

  include Git.FS.Make(M)

end

module IO_helper (Channel: V1_LWT.CHANNEL) = struct

  let write oc s =
    let buf = Cstruct.of_string s in
    Channel.write_buffer oc buf;
    Channel.flush oc

  let read_all ic =
    let len = 4096 in
    let res = Buffer.create len in
    let rec aux () =
      Channel.read_some ~len ic >>= fun buf ->
      match Cstruct.len buf with
      | 0 -> Lwt.return_unit
      | i ->
        Buffer.add_string res (Cstruct.to_string buf);
        if len = i then Lwt.return_unit
        else aux ()
    in
    aux () >>= fun () ->
    Lwt.return (Buffer.contents res)

  let read_exactly ic n =
    let res = Bytes.create n in
    let rec aux off =
      if off >= n then Lwt.return_unit
      else (
        Channel.read_some ~len:(n-off) ic >>= fun buf ->
        match Cstruct.len buf with
        | 0 -> Lwt.return_unit
        | i ->
          Cstruct.blit_to_string buf 0 res off i;
          aux (off + i)
      ) in
    aux 0 >>= fun () ->
    Lwt.return res

  let flush _ = Lwt.return_unit

end

(* channel with functional constructors. *)
module Fchannel = Channel.Make(Fflow)

module In_channel = struct
  include Fchannel
  let make ?close input =
    create (Fflow.make ?close ~input ())
end

module Out_channel = struct
  include Fchannel
  let make ?close output =
    create (Fflow.make ?close ~output ())
end

(* Cohttp IO with functional input/channel constructors *)
module FIO = Cohttp_mirage_io.Make(Fchannel)

(* hanlde the git:// connections *)
module Git_protocol = struct

  module Flow = Conduit_mirage.Flow
  module Channel = Channel.Make(Flow)
  include IO_helper (Channel)

  let with_connection (resolver, conduit) uri ?init fn =
    assert (Git.Sync.protocol uri = `Ok `Git);
    Log.debug "Connecting to %s" (Uri.to_string uri);
    Resolver_lwt.resolve_uri ~uri resolver >>= fun endp ->
    Conduit_mirage.client endp >>= fun client ->
    Conduit_mirage.connect conduit client >>= fun flow ->
    let ic = Channel.create flow in
    let oc = Channel.create flow in
    Lwt.finalize
      (fun () ->
         begin match init with
           | None   -> Lwt.return_unit
           | Some s -> write oc s
         end >>= fun () ->
         fn (ic, oc))
      (fun () -> Channel.close ic)

end

(* hanlde the http(s):// connections *)
module Smart_HTTP = struct

  module Conduit_channel = Channel.Make(Conduit_mirage.Flow)
  module HTTP_IO = Cohttp_mirage_io.Make(Conduit_channel)
  module Net = struct
    module IO = HTTP_IO
    type ctx = { resolver: Resolver_lwt.t; conduit: Conduit_mirage.t; }
    let sexp_of_ctx { resolver; _ } = Resolver_lwt.sexp_of_t resolver
    let default_ctx = {
      resolver = Resolver_mirage.localhost;
      conduit = Conduit_mirage.empty;
    }
    let connect_uri ~ctx uri =
      Resolver_lwt.resolve_uri ~uri ctx.resolver >>= fun endp ->
      Conduit_mirage.client endp >>= fun client ->
      Conduit_mirage.connect ctx.conduit client >>= fun flow ->
      let ch = Conduit_channel.create flow in
      Lwt.return (flow, ch, ch)
    let close_in _ = ()
    let close_out oc = Lwt.async (fun () -> Conduit_channel.close oc)
    let close _ oc = Lwt.async (fun () -> Conduit_channel.close oc)
  end
  module Request = Cohttp_lwt.Make_request(HTTP_IO)
  module Response = Cohttp_lwt.Make_response(HTTP_IO)
  module HTTP = struct
    include Cohttp_lwt.Make_client(HTTP_IO)(Net)
    module Request = Request
    module Response = Response
    let oc x = x
    let ic x = x
    let close_in = Net.close_in
    let close_out oc = Net.close () oc
  end

  type ctx = HTTP.ctx

  include IO_helper(Fchannel)

  module HTTP_fn = Git_http.Flow(HTTP)(In_channel)(Out_channel)
  let with_conduit ctx ?init uri fn =
    Net.connect_uri ~ctx uri >>= fun (_, ic, oc) ->
    Lwt.finalize
      (fun () ->
         begin match init with
           | None   -> Lwt.return_unit
           | Some s -> HTTP_IO.write oc s
         end >>= fun () ->
         fn (ic, oc))
      (fun () -> Conduit_channel.close ic)

  let with_connection (ctx:ctx) (uri:Uri.t) ?init fn =
    assert (Git.Sync.protocol uri =`Ok `Smart_HTTP);
    HTTP_fn.with_http ?init (with_conduit ctx ?init:None) uri fn

  module Flow = Fflow
  module Channel = Fchannel

end

module IO = struct

  module G = Git_protocol
  module H = Smart_HTTP

  type ctx = Resolver_lwt.t * Conduit_mirage.t

  module Flow = struct
    type 'a io = 'a Lwt.t
    type buffer = Cstruct.t
    module G = G.Flow
    module H = H.Flow
    type flow = [`Git of G.flow | `HTTP of H.flow ]
    type error = [ `Git of G.error | `HTTP of H.error ]
    let error_message = function
      | `Git e  -> "git: " ^ G.error_message e
      | `HTTP e -> "http: " ^ H.error_message e
    let git_err f t =
      f t >>= function
      | `Error (x:G.error) -> Lwt.return (`Error (`Git x))
      | `Ok x -> Lwt.return (`Ok x)
      | `Eof -> Lwt.return `Eof
    let http_err f t =
      f t >>= function
      | `Error (x:H.error) -> Lwt.return (`Error (`HTTP x))
      | `Ok x -> Lwt.return (`Ok x)
      | `Eof -> Lwt.return `Eof
    let read = function
      | `Git g -> git_err G.read g
      | `HTTP h -> http_err H.read h
    let write t v = match t with
      | `Git g -> git_err (G.write g) v
      | `HTTP h -> http_err (H.write h) v
    let writev t v = match t with
      | `Git g -> git_err (G.writev g) v
      | `HTTP h -> http_err (H.writev h) v
    let close = function
      | `Git g -> G.close g
      | `HTTP h -> H.close h
  end

  module Channel = Channel.Make(Flow)
  include IO_helper(Channel)
  type ic = Channel.t
  type oc = Channel.t

  let with_connection ?ctx uri ?init fn =
    let resolver, conduit = match ctx with
      | Some x -> x
      | None   ->
        let { H.Net.resolver; conduit } = H.Net.default_ctx in
        resolver, conduit
    in
    match Git.Sync.protocol uri with
    | `Ok `SSH -> failwith "GIT+SSH is not supported with Mirage"
    | `Ok `Git ->
      let fn (ic, oc) =
        let ic = `Git (G.Channel.to_flow ic) in
        let oc = `Git (G.Channel.to_flow oc) in
        fn (Channel.create ic, Channel.create oc)
      in
      G.with_connection (resolver, conduit) ?init uri fn
    | `Ok `Smart_HTTP ->
      let ctx = { H.Net.resolver; conduit } in
      let fn (ic, oc) =
        let ic = `HTTP (H.Channel.to_flow ic) in
        let oc = `HTTP (H.Channel.to_flow oc) in
        fn (Channel.create ic, Channel.create oc)
      in
      H.with_connection ctx ?init:None uri fn
    | `Not_supported x ->
      Lwt.fail (Failure ("Scheme " ^ x ^ " not supported yet"))
    | `Unknown ->
      Lwt.fail (Failure ("Unknown protocol. Must supply a scheme like git://"))

end

module Sync = struct
  module IO = IO
  module Result = Git.Sync.Result
  module Make = Git.Sync.Make(IO)
end
