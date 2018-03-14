module Net = Net
module SHA1: Git.HASH
module FS = Fs

module Sync (C: Net.CONDUIT) (S: Git.S): Git.Sync.S
  with module Store = S
   and module Net = Net.Make(C)

module Store (X: Mirage_fs_lwt.S): sig
  include Git.Store.S with module Hash = SHA1

  val v:
    ?temp_dir:Fpath.t ->
    ?current_dir:Fpath.t ->
    ?root:Fpath.t ->
    ?dotgit:Fpath.t ->
    ?compression:int ->
    ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t) ->
    X.t -> (t, error) result Lwt.t
end
