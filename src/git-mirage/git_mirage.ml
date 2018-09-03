module Net = Net
module SHA1 = Git.Hash.SHA1
module FS = Fs

module Store (X: Mirage_fs_lwt.S) = struct
  module F = Fs.Make(X)
  module S = Git.Store.Make(SHA1)(F)(Git.Inflate)(Git.Deflate)
  include S

  let v ?current_dir ?dotgit ?compression ?buffer fs root =
    let fs = F.v ?current_dir fs in
    v ?dotgit ?compression ?buffer fs root
end

module Sync (C: Net.CONDUIT) = Git.Sync.Make(Net.Make(C))
(* module HTTP = Git_http.Sync.Make(Client) *)
