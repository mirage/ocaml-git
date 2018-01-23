module Make (FS: Mirage_fs_lwt.S): sig
  include Git.FS
  val v: ?temp_dir:Fpath.t -> ?current_dir:Fpath.t -> FS.t -> t
end
