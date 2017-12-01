module FS = Fs
module Lock = Lock
module Net = Net
module SHA1: Git.HASH

module Make (L: Git.LOCK) (Fs: Git.FS with type File.lock = L.elt)
  : Git.Store.S
    with module Hash = SHA1
     and module Lock = L
     and module FS = Fs
