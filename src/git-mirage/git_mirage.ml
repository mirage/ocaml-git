module Net = Net
module FS = Fs

module Store (X: Mirage_fs_lwt.S) = struct
  module F = Fs.Make(X)
  module I = Git.Inflate
  module D = Git.Deflate
  module S = Git.Store.Make(Digestif.SHA1)(F)(I)(D)

  include S

  let v ?current_dir ?dotgit ?compression ?buffer fs root =
    let fs = F.v ?current_dir fs in
    v ?dotgit ?compression ?buffer fs root
end

module Sync (C: Net.CONDUIT) = Git.Sync.Make(Net.Make(C))
(* module HTTP = Git_http.Sync.Make(Client) *)
