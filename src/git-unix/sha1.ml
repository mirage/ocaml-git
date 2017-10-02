type t = Bytes.t

module Digest =
struct
  type t = Bytes.t
  type ctx = Digestif.SHA1.Bytes.ctx
  type buffer = Cstruct.t

  let init () = Digestif.SHA1.Bytes.init ()
  let feed ctx cs = Digestif.SHA1.Bytes.feed_bigstring ctx (Cstruct.to_bigarray cs)
  let get ctx = Digestif.SHA1.Bytes.get ctx

  let length = Digestif.SHA1.digest_size
end

let of_string = Bytes.of_string
let to_string = Bytes.to_string
let get = Bytes.get

type hex = string

let of_hex buf = Digestif.SHA1.Bytes.of_hex (Bytes.unsafe_of_string buf)
let to_hex x = Digestif.SHA1.Bytes.to_hex x |> Bytes.to_string

module Map = Map.Make(Bytes)
module Set = Set.Make(Bytes)

let compare = Bytes.compare
let equal = Bytes.equal
let hash : Bytes.t -> int = Hashtbl.hash
let pp ppf hash = Fmt.string ppf (to_hex hash)
