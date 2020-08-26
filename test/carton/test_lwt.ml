open Lwt.Infix

let map _ fd ~pos len =
  let fd = Lwt_unix.unix_file_descr fd in
  let payload =
    Mmap.V1.map_file fd ~pos Bigarray.char Bigarray.c_layout false [| len |]
  in
  Lwt.return (Bigarray.array1_of_genarray payload)

let yield_map root fd ~pos len = map root fd ~pos len

let create root path =
  let path = Fpath.(root // path) in

  let rec process () =
    Lwt_unix.openfile (Fpath.to_string path)
      Unix.[ O_RDWR; O_APPEND; O_CREAT ]
      0o644
    >>= fun fd -> Lwt.return_ok fd
  and error = function
    | Unix.Unix_error (Unix.ENOENT, _, _) | Unix.Unix_error (Unix.EACCES, _, _)
      ->
        Lwt.return_error
          (`Msg (Fmt.strf "Impossible to open %a." Fpath.pp path))
    | Unix.Unix_error (Unix.EINTR, _, _) -> Lwt.catch process error
    | exn -> Lwt.fail exn
  in
  Lwt.catch process error

let append _ fd str =
  let rec go off len =
    let process () =
      Lwt_unix.write_string fd str off len >>= fun len' ->
      if len = len' then Lwt.return () else go (off + len') (len - len')
    in
    let error = function
      | Unix.Unix_error (Unix.EINTR, _, _) -> go off len
      | exn -> Lwt.fail exn
    in
    Lwt.catch process error
  in
  go 0 (String.length str)

let close _ fd =
  let rec process () = Lwt_unix.close fd >>= fun () -> Lwt.return_ok ()
  and error = function
    | Unix.Unix_error (Unix.EINTR, _, _) -> Lwt.catch process error
    | exn -> Lwt.fail exn
  in
  Lwt.catch process error

module Thin = Carton_lwt.Thin.Make (Uid)

let access = { Thin.create; Thin.append; Thin.map = yield_map; Thin.close }

let safely_open path =
  let rec process () =
    Lwt_unix.openfile (Fpath.to_string path) Unix.[ O_RDONLY ] 0o400
  and error = function
    | Unix.Unix_error (Unix.EINTR, _, _) -> Lwt.catch process error
    | exn -> Lwt.fail exn
  in
  Lwt.catch process error

let safely_close fd =
  let rec process () = Lwt_unix.close fd
  and error = function
    | Unix.Unix_error (Unix.EINTR, _, _) -> Lwt.catch process error
    | exn -> Lwt.fail exn
  in
  Lwt.catch process error

let safely_read fd tmp off len =
  let rec process () = Lwt_unix.read fd tmp off len
  and error = function
    | Unix.Unix_error (Unix.EINTR, _, _) -> Lwt.catch process error
    | exn -> Lwt.fail exn
  in
  Lwt.catch process error

let stream_of_file path =
  let stream fd () =
    let tmp = Bytes.create De.io_buffer_size in
    safely_read fd tmp 0 (Bytes.length tmp) >>= function
    | 0 -> safely_close fd >>= fun () -> Lwt.return_none
    | len ->
        let res = Bytes.sub_string tmp 0 len in
        Lwt.return_some (res, 0, len)
  in
  safely_open path >|= stream

let digest ~kind ?(off = 0) ?len buf =
  let len =
    match len with Some len -> len | None -> Bigstringaf.length buf - off
  in
  let ctx = Digestif.SHA1.empty in

  let ctx =
    match kind with
    | `A -> Digestif.SHA1.feed_string ctx (Fmt.strf "commit %d\000" len)
    | `B -> Digestif.SHA1.feed_string ctx (Fmt.strf "tree %d\000" len)
    | `C -> Digestif.SHA1.feed_string ctx (Fmt.strf "blob %d\000" len)
    | `D -> Digestif.SHA1.feed_string ctx (Fmt.strf "tag %d\000" len)
  in
  let ctx = Digestif.SHA1.feed_bigstring ctx ~off ~len buf in
  Digestif.SHA1.get ctx

let test_map_yield =
  Alcotest.test_case "map-yield" `Quick @@ fun () ->
  let root = Fpath.v "."
  and pack0 = Fpath.v "bomb.pack"
  and pack1 = Fpath.v "check.pack" in

  let fiber =
    stream_of_file pack0 >>= Thin.verify ~threads:4 ~digest root pack1 access
  in
  match Lwt_main.run fiber with
  | Ok (_n, [], [], _resolveds, _, _uid) ->
      Alcotest.(check pass) "map-yield" () ()
  | Ok _ -> Alcotest.failf "Invalid thin-pack."
  | Error (`Msg err) -> Alcotest.failf "%s" err
