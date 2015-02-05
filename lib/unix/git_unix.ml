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

open Lwt
open Git
open Printf

module Log = Log.Make(struct let section = "unix" end)

(* Pool of opened files *)
let openfile_pool = Lwt_pool.create 200 (fun () -> return_unit)

let mkdir_pool = Lwt_pool.create 1 (fun () -> return_unit)

module M = struct
  type ic = Lwt_io.input_channel

  type oc = Lwt_io.output_channel

  let write oc s =
    Lwt_io.write oc s

  let flush oc =
    Lwt_io.flush oc

  let with_ssh_process ?init uri fn =
    let host = match Uri.host uri with
      | None   -> "localhost"
      | Some x -> x
    in
    let user = match Uri.userinfo uri with
      | None   -> ""
      | Some u -> u ^ "@"
    in
    let cmd = match init with
      | None   -> [| "ssh"; user ^ host; |]
      | Some x -> [| "ssh"; user ^ host; x |]
    in
    Log.info "Executing '%s'" (String.concat " " (Array.to_list cmd));
    let env = Unix.environment () in
    let p = Lwt_process.open_process_full ~env ("ssh", cmd) in
    Lwt.finalize
      (fun () -> fn (p#stdout, p#stdin))
      (fun () -> let _ = p#close in return_unit)

  let with_conduit ?init uri fn =
    Log.debug "Connecting to %s" (Uri.to_string uri);
    let resolver = Resolver_lwt_unix.system in
    Resolver_lwt.resolve_uri ~uri resolver >>= fun endp ->
    let ctx = Conduit_lwt_unix.default_ctx in
    Conduit_lwt_unix.endp_to_client ~ctx endp >>= fun client ->
    Conduit_lwt_unix.connect ~ctx client >>= fun (_flow, ic, oc) ->
    Lwt.finalize
      (fun () ->
         begin match init with
           | None   -> return_unit
           | Some s -> write oc s
         end >>= fun () ->
         fn (ic, oc))
      (fun ()  -> Lwt_io.close ic)

  exception Redirect of Uri.t

  let rec http_call ?headers ?(redirects=0) meth uri fn =
    let headers = match headers with None -> Cohttp.Header.init () | Some h -> h in
    let callback (ic, oc) =
      let req = match meth with
        | `GET ->
          Cohttp.Request.make_for_client ~headers ~chunked:false
            (meth :> Cohttp.Code.meth) uri
        | `POST ->
          Cohttp.Request.make_for_client ~headers ~chunked:true
            (meth :> Cohttp.Code.meth) uri
      in
      let http_oc =
        match meth with
        | `GET  -> oc
        | `POST ->
          let writer =
            Cohttp_lwt_unix.Request.make_body_writer ~flush:true req oc
          in
          Lwt_io.make ~mode:Lwt_io.output ~close:(fun () -> Lwt_io.close oc)
            (fun bytes off len ->
               let chunk = Bytes.create len in
               Lwt_bytes.blit_to_bytes bytes off chunk 0 len;
               Cohttp_lwt_unix.Request.write_body writer chunk >>= fun () ->
               return len)
      in
      let flush_http_oc () =
        Log.debug "Closing output connection";
        Cohttp_lwt_unix.Request.write_footer req oc
      in
      let http_ic =
        let reader = ref None in
        let old_chunk = ref None in
        let read reader bytes off len =
          let write chunk =
            let blit len =
              Lwt_bytes.blit_from_bytes chunk 0 bytes off len;
              Log.debug "refill: actual-len=%d" len;
              Lwt.return len
            in
            let n = String.length chunk in
            if n <= len then blit n
            else
              let tl = String.sub chunk len (n - len - 1) in
              old_chunk := Some tl;
              blit len
          in
          match !old_chunk with
          | Some c -> write c
          | None ->
            Cohttp_lwt_unix.Response.read_body_chunk reader >>= function
            | Cohttp.Transfer.Done -> Lwt.return 0
            | Cohttp.Transfer.Chunk chunk -> write chunk
            | Cohttp.Transfer.Final_chunk chunk -> write chunk
        in
        Lwt_io.make ~mode:Lwt_io.input ~close:(fun () -> Lwt_io.close ic)
          (fun bytes off len ->
             match !reader with
             | None ->
               begin
                 flush_http_oc () >>= fun () ->
                 Cohttp_lwt_unix.Response.read ic >>= function
                 | `Ok r ->
                   let status = Cohttp_lwt_unix.Response.status r in
                   let status_code = Cohttp.Code.code_of_status status in
                   let status = Cohttp.Code.string_of_status status in
                   if Cohttp.Code.is_redirection status_code then (
                     let uri =
                       try
                         Cohttp_lwt_unix.Response.headers r
                         |> Cohttp.Header.to_list
                         |> List.assoc "location"
                         |> Uri.of_string
                       with Not_found ->
                         failwith status
                     in
                     fail (Redirect uri)
                   ) else if Cohttp.Code.is_success status_code then (
                     let r = Cohttp_lwt_unix.Response.make_body_reader r ic in
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
      Cohttp_lwt_unix.Request.write_header req oc >>= fun () ->
      Lwt.catch
        (fun () -> fn (http_ic, http_oc))
        (function
          | Redirect uri ->
            Lwt_io.close http_ic >>= fun () ->
            let redirects = redirects + 1 in
            if redirects > 10 then fail (Failure "Too many redirects")
            else http_call ~headers ~redirects meth uri fn
          | e -> fail e)
    in
    with_conduit uri callback

  let with_http ?init uri fn =
    Log.debug "HTTP connecting to %s" (Uri.to_string uri);
    let headers = match init with
      | None -> Cohttp.Header.of_list []
      | Some s ->
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
    http_call ~headers meth uri fn

  let with_connection uri ?init fn =
    match Sync.protocol uri with
    | `Ok `SSH -> with_ssh_process ?init uri fn
    | `Ok `Git -> with_conduit ?init uri fn
    | `Ok `Smart_HTTP -> with_http ?init uri fn
    | `Not_supported x ->
      fail (Failure ("Scheme " ^ x ^ " not supported yet"))
    | `Unknown ->
      fail (Failure ("Unknown protocol. Must supply a scheme like git://"))

  let read_all ic =
    let len = 64_1024 in
    let buf = Bytes.create len in
    let res = Buffer.create len in
    let rec aux () =
      Lwt_io.read_into ic buf 0 len >>= function
      | 0 -> return_unit
      | i -> Buffer.add_substring res buf 0 i;
        if len = i then return_unit
        else aux ()
    in
    aux () >>= fun () ->
    return (Buffer.contents res)

  let read_exactly ic n =
    let res = Bytes.create n in
    Lwt_io.read_into_exactly ic res 0 n >>= fun () ->
    return res

end

module D = struct

  let protect_exn = function
    | Unix.Unix_error _ as e -> Lwt.fail (Failure (Printexc.to_string e))
    | e -> Lwt.fail e

  let protect f x =
    Lwt.catch (fun () -> f x) protect_exn

  let mkdir dirname =
    let rec aux dir =
      if Sys.file_exists dir then return_unit
      else (
        aux (Filename.dirname dir) >>= fun () ->
        Log.debug "mkdir %s" dir;
        protect (Lwt_unix.mkdir dir) 0o755;
      ) in
    Lwt_pool.use mkdir_pool (fun () -> aux dirname)

  let list_files kind dir =
    Lwt_pool.use openfile_pool (fun () ->
        if Sys.file_exists dir then (
          let s = Lwt_unix.files_of_directory dir in
          let s = Lwt_stream.filter (fun s -> s <> "." && s <> "..") s in
          let s = Lwt_stream.map (Filename.concat dir) s in
          let s = Lwt_stream.filter kind s in
          Lwt_stream.to_list s >>= fun l ->
          return l
        ) else
          return_nil
      )

  let directories dir =
    list_files (fun f ->
        try Sys.is_directory f with Sys_error _ -> false
      ) dir

  let files dir =
    list_files (fun f ->
        try not (Sys.is_directory f) with Sys_error _ -> false
      ) dir

  let rec_files dir =
    let rec aux accu dir =
      directories dir >>= fun ds ->
      files dir       >>= fun fs ->
      Lwt_list.fold_left_s aux (fs @ accu) ds in
    aux [] dir

  let write_cstruct fd b =
    let rec rwrite fd buf ofs len =
      Lwt_bytes.write fd buf ofs len >>= fun n ->
      if len = 0 then fail End_of_file
      else if n < len then rwrite fd buf (ofs + n) (len - n)
      else return_unit in
    match Cstruct.len b with
    | 0   -> return_unit
    | len -> rwrite fd (Cstruct.to_bigarray b) 0 len

  let with_write_file ?temp_dir file fn =
    begin match temp_dir with
      | None   -> Lwt.return_unit
      | Some d -> mkdir d
    end >>= fun () ->
    let dir = Filename.dirname file in
    mkdir dir >>= fun () ->
    let tmp = Filename.temp_file ?temp_dir (Filename.basename file) "write" in
    Lwt_pool.use openfile_pool (fun () ->
        Log.info "Writing %s (%s)" file tmp;
        Lwt_unix.(openfile tmp [O_WRONLY; O_NONBLOCK; O_CREAT; O_TRUNC] 0o644) >>= fun fd ->
        Lwt.finalize
          (fun () -> protect fn fd >>= fun () -> Lwt_unix.rename tmp file)
          (fun _  -> Lwt_unix.close fd)
      )

  let write_file file ?temp_dir b =
    with_write_file file ?temp_dir (fun fd -> write_cstruct fd b)

  let read_file file =
    Unix.handle_unix_error (fun () ->
        Lwt_pool.use openfile_pool (fun () ->
            Log.info "Reading %s" file;
            let fd = Unix.(openfile file [O_RDONLY; O_NONBLOCK] 0o644) in
            let ba = Lwt_bytes.map_file ~fd ~shared:false () in
            Unix.close fd;
            return (Cstruct.of_bigarray ba)
          ))
      ()

  let realdir dir =
    if Sys.file_exists dir then (
      let d = Sys.getcwd () in
      Unix.chdir dir;
      let e = Sys.getcwd () in
      Sys.chdir d;
      e
    ) else dir

  let realpath file =
    let r =
      if Sys.file_exists file && Sys.is_directory file then
        realdir file
      else
        Filename.concat
          (realdir (Filename.dirname file))
          (Filename.basename file) in
    return r

  let stat_info path =
    let open Index in
    let stats = Unix.stat path in
    let ctime = { lsb32 = Int32.of_float stats.Unix.st_ctime; nsec = 0l } in
    let mtime = { lsb32 = Int32.of_float stats.Unix.st_mtime; nsec = 0l } in
    let dev = Int32.of_int stats.Unix.st_dev in
    let inode = Int32.of_int stats.Unix.st_ino in
    let mode = match stats.Unix.st_kind, stats.Unix.st_perm with
      | Unix.S_REG, p -> if p land 0o100 = 0o100 then `Exec else `Normal
      | Unix.S_LNK, _ -> `Link
      | k, p ->
        let kind = match k with
          | Unix.S_REG -> "REG"
          | Unix.S_DIR -> "DIR"
          | Unix.S_CHR -> "CHR"
          | Unix.S_BLK -> "BLK"
          | Unix.S_LNK -> "LNK"
          | Unix.S_FIFO -> "FIFO"
          | Unix.S_SOCK -> "SOCK"
        in
        let perm = Printf.sprintf "%o" p in
        let error =
          Printf.sprintf "%s: not supported kind of file [%s, %s]."
            path kind perm
        in
        failwith error
    in
    let uid = Int32.of_int stats.Unix.st_uid in
    let gid = Int32.of_int stats.Unix.st_gid in
    let size = Int32.of_int stats.Unix.st_size in
    { ctime; mtime; dev; inode; uid; gid; mode; size }

  let file_exists f =
    return (Sys.file_exists f)

  let remove f =
    let _ = Sys.command (sprintf "rm -rf %s" f) in
    return_unit

  let chmod f i =
    return (Unix.chmod f i)

  let getcwd () =
    return (Sys.getcwd ())

end

module Sync = struct
  module IO = M
  module Result = Sync.Result
  module Make = Sync.Make(M)
end

module FS = struct
  module IO = D
  include Git.FS.Make(D)
end
