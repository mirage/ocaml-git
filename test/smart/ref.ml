type t = string

let v x = x
let to_string x = x
let equal a b = String.equal a b
let pp = Fmt.string
let segs v = Astring.String.cuts ~empty:false ~sep:"/" v
