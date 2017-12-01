type t = Bytes.t
type hex = string

module Make (D: Digestif.S) = struct
  type nonrec t = t
  type nonrec hex = hex
  module Digest = struct
    type t = Bytes.t
    type ctx = D.Bytes.ctx
    type buffer = Cstruct.t

    let init () = D.Bytes.init ()
    let feed ctx cs = D.Bytes.feed_bigstring ctx (Cstruct.to_bigarray cs)
    let get ctx = D.Bytes.get ctx

    let length = D.digest_size
  end

  let of_string = Bytes.of_string
  let to_string = Bytes.to_string
  let get = Bytes.get

  let of_hex buf = D.Bytes.of_hex (Bytes.unsafe_of_string buf)
  let to_hex x = D.Bytes.to_hex x |> Bytes.to_string

  module Map = Map.Make(Bytes)
  module Set = Set.Make(Bytes)

  let compare = Bytes.compare
  let equal = Bytes.equal
  let hash : Bytes.t -> int = Hashtbl.hash
  let pp ppf hash = Fmt.string ppf (to_hex hash)
end

(* XXX: this breaks as jbuilder doesn't the linking trick yet *)
(* module SHA1 =  Make(Digestif.SHA1) *)
