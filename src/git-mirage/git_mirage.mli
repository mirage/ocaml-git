module Net = Net
module SHA1: Git.HASH
module FS = Fs

module Sync (C: Net.CONDUIT) (S: Git.S): Git.Sync.S
  with module Store = S
   and module Net = Net.Make(C)

module Store (G: FS.GAMMA) (X: FS.S): Git.Store.S with module Hash = SHA1
