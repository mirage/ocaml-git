open Sigs

type 'uid commit = { root : 'uid; preds : 'uid list }

type none = Leaf

type ('uid, 'preds) kind

val commit : ('uid, 'uid commit) kind

val tree : ('uid, 'uid list) kind

val blob : ('uid, none) kind

val tag : ('uid, 'uid) kind

type 'uid t

val make : kind:('uid, 'preds) kind -> 'preds -> ?ts:int64 -> 'uid -> 'uid t

val get_limits :
  compare:('uid -> 'uid -> int) ->
  ('uid * 'ref * bool) list ->
  ('uid, 'ref) Smart.Commands.command list ->
  'uid list * 'uid list

val commands :
  's scheduler ->
  capabilities:Smart.Capability.t list ->
  equal:('ref -> 'ref -> bool) ->
  deref:('store -> 'ref -> ('uid option, 's) io) ->
  'store ->
  [ `Create of 'ref | `Delete of 'ref | `Update of 'ref * 'ref ] list ->
  ('uid * 'ref * bool) list ->
  (('uid, 'ref) Smart.Commands.t option, 's) io

val get_uncommon_objects :
  's scheduler ->
  compare:('uid -> 'uid -> int) ->
  ('uid, 'ref, 'uid t, 'g, 's) access ->
  ('uid, 'uid t, 'g) store ->
  exclude:'uid list ->
  sources:'uid list ->
  ('uid list, 's) io
