module Net = Net
module SHA1 = Git.Hash.Make(Digestif.SHA1)
module FS = Fs

module Store (G: FS.GAMMA) (X: FS.S) = struct
  module F = Fs.Make(G)(X)
  module S = Git.Store.FS(SHA1)(Git.Mem.Lock)(F)(Git.Inflate)(Git.Deflate)
  include S
end

module Sync (C: Net.CONDUIT) = Git.Sync.Make(Net.Make(C))
(* module HTTP = Git_http.Sync.Make(Client) *)
