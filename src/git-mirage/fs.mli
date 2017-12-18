module type GAMMA = sig
  val current : Fpath.t
  val temp    : Fpath.t
end

module type S = sig
  include Mirage_fs_lwt.S
  val connect : unit -> t Lwt.t
  (* XXX: the constructor should not be there *)
end

module Make (Gamma: GAMMA) (FS: S): Git.FS
  with type File.lock = Git.Mem.Lock.elt
