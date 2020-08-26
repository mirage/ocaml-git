type t = Digestif.SHA1.t
type ctx = Digestif.SHA1.ctx

let empty = Digestif.SHA1.empty
let feed = Digestif.SHA1.feed_bigstring
let get = Digestif.SHA1.get
let hash = Hashtbl.hash
let length = Digestif.SHA1.digest_size
let equal = Digestif.SHA1.equal
let pp = Digestif.SHA1.pp
let of_raw_string = Digestif.SHA1.of_raw_string
let to_raw_string = Digestif.SHA1.to_raw_string
let of_hex = Digestif.SHA1.of_hex
let to_hex = Digestif.SHA1.to_hex
let compare = Digestif.SHA1.unsafe_compare
let null = Digestif.SHA1.digest_string ""
