include Git.S.LOCK
  with type +'a io = 'a Lwt.t
   and type key = Fpath.t
   and type elt = Lwt_mutex.t
