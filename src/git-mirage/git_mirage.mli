module FS = Fs

module Net = Net

module SHA1: Git.HASH

module Sync (C: Net.CONDUIT) (S: Git.S): Git.Sync.S
  with module Store = S
   and module Net = Net.Make(C)

module Make (L: Git.LOCK) (Fs: Git.FS with type File.lock = L.elt)
  : Git.Store.S
    with module Hash = SHA1
     and module Lock = L
     and module FS = Fs
