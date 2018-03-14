module Net = Net
module SHA1 = Git.Hash.Make(Digestif.SHA1)
module FS = Fs

module Store (X: Mirage_fs_lwt.S) = struct
  module F = Fs.Make(X)
  module S = Git.Store.Make(SHA1)(F)(Git.Inflate)(Git.Deflate)
  include S

  let v ?temp_dir ?current_dir ?root ?dotgit ?compression ?buffer fs =
    let fs = F.v ?temp_dir ?current_dir fs in
    create ?root ?dotgit ?compression ?buffer fs
end

module Sync (C: Net.CONDUIT) = Git.Sync.Make(Net.Make(C))
(* module HTTP = Git_http.Sync.Make(Client) *)
