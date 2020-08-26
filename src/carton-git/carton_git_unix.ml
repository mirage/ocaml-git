open Lwt.Infix
module Bigarray = Bigarray_compat

module Store = struct
  type 'a rd = < rd : unit ; .. > as 'a

  type 'a wr = < wr : unit ; .. > as 'a

  type 'a mode =
    | Rd : < rd : unit > mode
    | Wr : < wr : unit > mode
    | RdWr : < rd : unit ; wr : unit > mode

  type t = Fpath.t

  type uid = Fpath.t

  type 'a fd = Lwt_unix.file_descr

  type error = [ `Not_found of uid ]

  type +'a fiber = 'a Lwt.t

  let pp_error : error Fmt.t =
   fun ppf -> function
    | `Not_found uid -> Fmt.pf ppf "%a not found" Fpath.pp uid

  let create : type a. mode:a mode -> t -> uid -> (a fd, error) result fiber =
   fun ~mode root path ->
    let flags, perm =
      match mode with
      | Rd -> (Unix.[ O_RDONLY ], 0o400)
      | Wr -> (Unix.[ O_WRONLY; O_CREAT; O_APPEND ], 0o600)
      | RdWr -> (Unix.[ O_RDWR; O_CREAT; O_APPEND ], 0o600) in
    let path = Fpath.(root // path) in
    let process () =
      Lwt_unix.openfile (Fpath.to_string path) flags perm >>= fun fd ->
      Lwt.return_ok fd in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) ->
          Lwt.return_error (`Not_found path)
      | exn -> Lwt.fail exn in
    Lwt.catch process error

  let map : t -> 'm rd fd -> pos:int64 -> int -> Bigstringaf.t fiber =
   fun _ fd ~pos len ->
    let fd = Lwt_unix.unix_file_descr fd in
    let payload =
      Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
    in
    Lwt.return (Bigarray.array1_of_genarray payload)

  let close _ fd = Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()

  let length fd =
    Lwt_unix.LargeFile.fstat fd >>= fun st ->
    Lwt.return st.Unix.LargeFile.st_size

  let list root =
    Lwt_unix.opendir (Fpath.to_string root) >>= fun dh ->
    let rec go acc =
      Lwt.catch
        (fun () ->
          Lwt_unix.readdir dh >>= function
          | "." | ".." -> go acc
          | entry ->
          match Fpath.of_string entry with
          | Ok x -> if Fpath.has_ext "pack" x then go (x :: acc) else go acc
          | Error (`Msg _) -> (* ignore *) go acc)
        (function End_of_file -> Lwt.return acc | exn -> Lwt.fail exn) in
    go []
end

module Make (Uid : sig
  include Carton.UID

  val of_hex : string -> t

  val to_hex : t -> string
end) =
struct
  include Carton_git.Make (Carton_lwt.Scheduler) (Lwt) (Store) (Uid)

  let idx_major_uid_of_uid root uid =
    Fpath.(root / Fmt.strf "pack-%s.idx" (Uid.to_hex uid))

  let uid_of_major_uid path =
    let str = Fpath.basename (Fpath.rem_ext path) in
    match Astring.String.cut ~sep:"pack-" str with
    | Some ("", uid) -> Uid.of_hex uid
    | _ -> Fmt.failwith "Invalid path of major file: %a" Fpath.pp path

  let make store = make ~uid_of_major_uid ~idx_major_uid_of_uid store
end
