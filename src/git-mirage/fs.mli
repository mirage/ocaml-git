module Make (FS: Mirage_fs_lwt.S): sig
  include Git.FS
  val v: ?current_dir:Fpath.t -> FS.t -> t
end
