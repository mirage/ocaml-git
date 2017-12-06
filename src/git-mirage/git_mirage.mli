module Net = Net
module SHA1: Git.HASH
module FS = Fs

module Sync (C: Net.CONDUIT) (S: Git.S): Git.Sync.S
  with module Store = S
   and module Net = Net.Make(C)

module Store (G: FS.GAMMA) (X: FS.S): Git.Store.S
  with module Hash = SHA1
   and module Lock = Git.Mem.Lock
   and type FS.error = [ `System of string ]
   and type FS.File.lock = Git.Mem.Lock.elt
   and type FS.File.error = [ `Stat of string
                            | Mirage_fs.write_error
                            | Mirage_fs.error ]
   and type FS.Dir.error = [ `Destroy of string
                           | `Listdir of string
                           | `Stat of string
                           | `Path of string
                           | Mirage_fs.write_error ]
   and type FS.Mapper.error = Mirage_fs.error
