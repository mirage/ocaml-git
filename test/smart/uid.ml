include Digestif.SHA1

let length = Digestif.SHA1.digest_size
let compare = Digestif.SHA1.unsafe_compare
let feed = Digestif.SHA1.feed_bigstring
let null = Digestif.SHA1.digest_string ""
let hash = Hashtbl.hash
