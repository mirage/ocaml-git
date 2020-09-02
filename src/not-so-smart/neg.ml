open Find_common

type nonrec ('a, 's) raise = ('a, 's) raise

type nonrec configuration = configuration = {
  stateless : bool;
  mutable multi_ack : [ `None | `Some | `Detailed ];
  no_done : bool;
}

type nonrec 'uid hex = 'uid hex = {
  to_hex : 'uid -> string;
  of_hex : string -> 'uid;
  compare : 'uid -> 'uid -> int;
}

type 'uid negotiator = 'uid Default.t

let negotiator ~compare = Default.make ~compare
let run = run
let find_common = find_common
let tips = tips
