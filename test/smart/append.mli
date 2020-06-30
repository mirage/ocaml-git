include
  Smart_git.APPEND
    with type t = Fpath.t
     and type fd = Lwt_unix.file_descr
     and type uid = Fpath.t
     and type error = [ `Msg of string ]
     and type +'a fiber = 'a Lwt.t
