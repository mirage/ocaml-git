include Helper.BaseBytes

module Digest =
struct
  type t = Bytes.t
  type ctx = Digestif.SHA1.Bytes.ctx
  type buffer = Cstruct.t

  let init () = Digestif.SHA1.Bytes.init ()
  let feed ctx cs = Digestif.SHA1.Bytes.feed ctx (Cstruct.to_string cs |> Bytes.unsafe_of_string)
  let get ctx = Digestif.SHA1.Bytes.get ctx

  let length = Digestif.SHA1.digest_size
end

let of_string = Bytes.of_string
let to_string = Bytes.to_string
let get = Bytes.get

type hex = string

let of_hex buf = Digestif.SHA1.Bytes.of_hex (Bytes.unsafe_of_string buf)
let to_hex x = Digestif.SHA1.Bytes.to_hex x |> Bytes.to_string
