type _ hash = SHA1 : Digestif.SHA1.t hash

module Entry : sig
  type 'oid t

  val pp : pp_oid:'oid Fmt.t -> 'oid t Fmt.t
  val path : 'oid t -> Fpath.t
  val mode : 'oid t -> int
  val oid : 'oid t -> 'oid
end

type 'oid t

val exists : 'oid t -> Fpath.t -> bool
val find : 'oid t -> Fpath.t -> 'oid Entry.t option
val replace : 'oid t -> 'oid Entry.t -> unit

val add :
  hash:'oid hash -> Fpath.t -> 'oid t -> (unit, [> Rresult.R.msg ]) result

val rem : Fpath.t -> 'oid t -> unit
val make : ?version:int -> 'oid hash -> root:Fpath.t -> 'oid t

val load :
  hash:'oid hash ->
  root:Fpath.t ->
  Fpath.t ->
  ('oid t, [> Rresult.R.msg ]) result

val store :
  hash:'oid hash -> append:('fd -> Bigstringaf.t -> 'fd) -> 'fd -> 'oid t -> 'fd

val store_to_path :
  hash:'oid hash -> Fpath.t -> 'oid t -> (unit, [> Rresult.R.msg ]) result

type 'oid elt = [ `Tree of Fpath.t | `Blob of 'oid Entry.t ]

val fold :
  f:
    ([ 'oid elt | `Root ] ->
    'oid elt list ->
    'a ->
    ('a, ([> Rresult.R.msg ] as 'err)) result Lwt.t) ->
  'a ->
  'oid t ->
  ('a, 'err) result Lwt.t
