include Helper.BaseBytes

type ctx = Digestif.SHA1.Bytes.ctx
type buffer = Cstruct.t

let init        = Digestif.SHA1.Bytes.init
let feed ctx cs = Digestif.SHA1.Bytes.feed ctx (Cstruct.to_string cs |> Bytes.unsafe_of_string)
let get         = Digestif.SHA1.Bytes.get
let digestv l   = Digestif.SHA1.Bytes.digestv (List.map Cstruct.to_string l |> List.map Bytes.unsafe_of_string)

let length      = Digestif.SHA1.digest_size

let of_string   = Bytes.of_string
