module Net = Net
module FS = Fs

type endpoint = Net.endpoint =
  { uri: Uri.t
  ; conduit: Conduit_mirage.t
  ; resolver: Resolver_lwt.t
  ; headers: Cohttp.Header.t }

val endpoint :
     ?conduit:Conduit_mirage.t
  -> ?resolver:Resolver_lwt.t
  -> ?headers:Cohttp.Header.t
  -> Uri.t
  -> endpoint

module Sync (G : Git.S) : sig
  module Tcp : Git.Sync.S with module Store := G and type Endpoint.t = endpoint

  module Http :
    Git_http.Sync.S with module Store := G and type Client.endpoint = endpoint

  include Git.Sync.S with module Store := G and type Endpoint.t = endpoint
end

module Store (X : Mirage_fs_lwt.S) : sig
  include Git.Store.S with module Hash = Git.Hash.Make(Digestif.SHA1)

  val v :
       ?current_dir:Fpath.t
    -> ?dotgit:Fpath.t
    -> ?compression:int
    -> ?buffer:((buffer -> unit Lwt.t) -> unit Lwt.t)
    -> X.t
    -> Fpath.t
    -> (t, error) result Lwt.t
end
