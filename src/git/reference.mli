open Carton

type t

val of_string : string -> (t, [> `Msg of string ]) result

val v : string -> t

val add_seg : t -> string -> t

val append : t -> t -> t

val segs : t -> string list

val pp : t Fmt.t

val head : t

val ( / ) : t -> string -> t

val ( // ) : t -> t -> t

val to_string : t -> string

val equal : t -> t -> bool

val compare : t -> t -> int

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

type 'uid contents = Uid of 'uid | Ref of t

val equal_contents :
  equal:('uid -> 'uid -> bool) -> 'uid contents -> 'uid contents -> bool

val compare_contents :
  compare:('uid -> 'uid -> int) -> 'uid contents -> 'uid contents -> int

val pp_contents : pp:'uid Fmt.t -> 'uid contents Fmt.t

val uid : 'uid -> 'uid contents

val ref : t -> 'uid contents

module Packed : sig
  type 'uid elt = Ref of t * 'uid | Peeled of 'uid

  type 'uid packed = 'uid elt list

  type ('fd, 's) input_line = 'fd -> (string option, 's) io

  val load :
    's Carton.scheduler ->
    input_line:('fd, 's) input_line ->
    of_hex:(string -> 'uid) ->
    'fd ->
    ('uid packed, 's) io

  val get : t -> 'uid packed -> 'uid option

  val exists : t -> 'uid packed -> bool

  val remove : t -> 'uid packed -> 'uid packed
end

type ('t, 'uid, 'error, 's) store = {
  atomic_wr : 't -> t -> string -> ((unit, 'error) result, 's) io;
  atomic_rd : 't -> t -> ((string, 'error) result, 's) io;
  uid_of_hex : string -> 'uid option;
  uid_to_hex : 'uid -> string;
  packed : 'uid Packed.packed;
}

val resolve :
  's Carton.scheduler ->
  't ->
  ('t, 'uid, 'error, 's) store ->
  t ->
  (('uid, [> `Not_found of t | `Cycle ]) result, 's) io

val write :
  's Carton.scheduler ->
  't ->
  ('t, 'uid, 'error, 's) store ->
  t ->
  'uid contents ->
  ((unit, [> `Store of 'error ]) result, 's) io

val read :
  's Carton.scheduler ->
  't ->
  ('t, 'uid, 'error, 's) store ->
  t ->
  (('uid contents, [> `Not_found of t ]) result, 's) io

module type S = sig
  type hash

  type nonrec t = t

  type nonrec contents = hash contents
end
