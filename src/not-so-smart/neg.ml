type nonrec configuration = Find_common.configuration = {
  stateless : bool;
  mutable multi_ack : [ `None | `Some | `Detailed ];
  no_done : bool;
}

type nonrec 'uid hex = 'uid Find_common.hex = {
  to_hex : 'uid -> string;
  of_hex : string -> 'uid;
  compare : 'uid -> 'uid -> int;
}

type 'uid negotiator = 'uid Default.t

let make ~compare = Default.make ~compare
let find_common = Find_common.find_common
let tips = Find_common.tips
