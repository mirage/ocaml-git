module Net = Net
module FS = Fs

module Sync (C : Net.CONDUIT) (G : Git.S) :
  Git.Sync.S with module Store = G and module Endpoint = Git.Gri

module Store (X : Mirage_fs_lwt.S) : sig
  include Git.Store.S with module Hash = Git.Hash.Make(Digestif.SHA1)

  val v :
       ?current_dir:Fpath.t
    -> ?dotgit:Fpath.t
    -> ?compression:int
    -> ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t)
    -> X.t
    -> Fpath.t
    -> (t, error) result Lwt.t
end
