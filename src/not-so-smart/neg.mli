(** Negotiation engine used to fetch objects from a Git repository.

    This implementation does not have any Git logics or how to store properly a
    Git object. However, it implements the negotiation engine used to fetch
    objects from a Git repository.

    Finally, it needs fews primitives to properly receive Git objects from the
    state of a local Git repository:

    - [exists] which tells to us if an object exists or not.
    - [parents] to get parents of a commit.
    - [deref] to de-reference a given reference.
    - [locals] to get locals references of the Git repository.

    The user must give to us a light store which is able to keep some mutable
    values used by the negotiation engine.

    [find_common] talks directly to the remote Git repository. *)

open Sigs

type configuration = {
  stateless : bool;
  mutable multi_ack : [ `None | `Some | `Detailed ];
  no_done : bool;
}

type 'uid hex = {
  to_hex : 'uid -> string;
  of_hex : string -> 'uid;
  compare : 'uid -> 'uid -> int;
}

type ('a, 's) raise = exn -> ('a, 's) io
type 'uid negotiator

val make : compare:('uid -> 'uid -> int) -> 'uid negotiator

val run :
  's scheduler ->
  ('res, 's) raise ->
  ('flow, 'error, 's) flow ->
  'flow ->
  ('res, [ `Protocol of Smart.error ]) Smart.t ->
  ('res, 's) io

val find_common :
  's scheduler ->
  ('flow, 'error, 's) flow ->
  'flow ->
  configuration ->
  'uid hex ->
  ('uid, 'ref, 'uid * int ref * int64, 'g, 's) access ->
  ('uid, 'uid * int ref * int64, 'g) store ->
  'uid negotiator ->
  Smart.context ->
  ?deepen:[ `Depth of int | `Timestamp of int64 ] ->
  'uid list ->
  ([ `Continue of int | `Close ], 's) io

val tips :
  's scheduler ->
  ('uid, 'ref, 'uid * int ref * int64, 'g, 's) access ->
  ('uid, 'uid * int ref * int64, 'g) store ->
  'uid negotiator ->
  (unit, 's) io
