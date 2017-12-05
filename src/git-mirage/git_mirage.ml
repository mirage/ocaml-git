module FS = Fs
module Net = Net

module SHA1 = Git.Hash.Make(Digestif.SHA1)

module Make (L : Git.LOCK) (FS: Git.FS with type File.lock = L.elt) = struct
  include Git.Store.Make(SHA1)(L)(FS)(Git.Inflate)(Git.Deflate)
end

module Sync (C: Net.CONDUIT) = Git.Sync.Make(Net.Make(C))
(* module HTTP = Git_http.Sync.Make(Client) *)
