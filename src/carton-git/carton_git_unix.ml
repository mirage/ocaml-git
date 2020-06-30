open Lwt.Infix
module Bigarray = Bigarray_compat

module Store = struct
  type t = Fpath.t

  type uid = Fpath.t

  and fd = Lwt_unix.file_descr

  type error = [ `Not_found of uid ]

  type +'a fiber = 'a Lwt.t

  let pp_error : error Fmt.t =
   fun ppf -> function
    | `Not_found uid -> Fmt.pf ppf "%a not found" Fpath.pp uid

  let create root path =
    let path = Fpath.(root // path) in
    let process () =
      Lwt_unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o644
      >>= fun fd -> Lwt.return_ok fd in
    let error = function
      | Unix.Unix_error (Unix.EACCES, _, _) ->
          Lwt.return_error (`Not_found path)
      | exn -> Lwt.fail exn in
    Lwt.catch process error

  let map : t -> fd -> pos:int64 -> int -> Bigstringaf.t fiber =
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

module Make (Uid : Carton.UID) = struct
  include Carton_git.Make (Carton_lwt.Scheduler) (Lwt) (Store) (Uid)
end
