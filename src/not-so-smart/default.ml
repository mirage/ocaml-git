(** default[1] negotiator implementation

   [1] "default" as defined in the canonical git implementation in C,
       see https://github.com/git/git/tree/master/negotiator *)

open Sigs

type ('k, 'p, 't) psq =
  (module Psq.S with type k = 'k and type p = 'p and type t = 't)

type ('uid, 'g, 's) parents =
  'uid ->
  ('uid, 'uid * int ref * int64, 'g) store ->
  (('uid * int ref * int64) list, 's) io

type 'uid t =
  | State : {
      mutable rev_list : 'psq;
      psq : ('uid, 'uid * int ref * int64, 'psq) psq;
      mutable non_common_revs : int;
    }
      -> 'uid t

let _common = 1 lsl 2
let _common_ref = 1 lsl 3
let _seen = 1 lsl 4
let _popped = 1 lsl 5

let make : type uid. compare:(uid -> uid -> int) -> uid t =
 fun ~compare ->
  let module K = struct
    type t = uid

    let compare = compare
  end in
  let module P = struct
    type t = uid * int ref * int64

    let compare (_, _, a) (_, _, b) = Int64.compare b a
  end in
  let module Psq = Psq.Make (K) (P) in
  let rev_list = Psq.empty in
  let non_common_revs = 0 in
  State { rev_list; psq = (module Psq); non_common_revs }

let rev_list_push : type uid. uid t -> uid * int ref * int64 -> int -> unit =
 fun (State ({ rev_list; psq = (module Psq); non_common_revs } as state))
     (uid, p, ts) mark ->
  if !p land mark = 0 then p := !p lor mark;
  state.rev_list <- Psq.add uid (uid, p, ts) rev_list;
  if !p land _common = 0 then state.non_common_revs <- non_common_revs + 1

let rec mark_common :
    type g s uid.
    s scheduler ->
    parents:(uid, g, s) parents ->
    (uid, uid * int ref * int64, g) store ->
    uid t ->
    uid * int ref * int64 ->
    bool ->
    (unit, s) io =
 fun ({ bind; return } as scheduler) ~parents store
     (State ({ non_common_revs; _ } as state) as t) (uid, p, ts) only_ancestors ->
  let ( >>= ) = bind in

  if only_ancestors then p := !p lor _common;
  if !p land _seen = 0 then (
    rev_list_push t (uid, p, ts) _seen;
    return () )
  else (
    if (not only_ancestors) && !p land _popped = 0 then
      state.non_common_revs <- non_common_revs - 1;
    parents uid store
    >>=
    let rec go = function
      | [] -> return ()
      | (uid, p, ts) :: rest ->
          mark_common scheduler ~parents store t (uid, p, ts) false
          >>= fun () -> go rest
    in
    go )

let known_common :
    type g s uid.
    s scheduler ->
    parents:(uid, g, s) parents ->
    (uid, uid * int ref * int64, g) store ->
    uid t ->
    uid * int ref * int64 ->
    (unit, s) io =
 fun ({ return; _ } as scheduler) ~parents store t (uid, p, ts) ->
  if !p land _seen = 0 then (
    rev_list_push t (uid, p, ts) (_common_ref lor _seen);
    mark_common scheduler ~parents store t (uid, p, ts) true )
  else return ()

let tip t obj = rev_list_push t obj _seen

let ack :
    type g s uid.
    s scheduler ->
    parents:(uid, g, s) parents ->
    (uid, uid * int ref * int64, g) store ->
    uid t ->
    uid * int ref * int64 ->
    (bool, s) io =
 fun ({ bind; return } as scheduler) ~parents store t (uid, p, ts) ->
  let ( >>= ) = bind in

  let res = not (!p land _common = 0) in
  mark_common scheduler ~parents store t (uid, p, ts) false >>= fun () ->
  return res

let get_rev :
    type g s uid.
    s scheduler ->
    parents:(uid, g, s) parents ->
    (uid, uid * int ref * int64, g) store ->
    uid t ->
    (uid option, s) io =
 fun ({ bind; return } as scheduler) ~parents store
     (State ({ psq = (module Psq); _ } as state) as t) ->
  let ( >>= ) = bind in

  let rec go () =
    if state.non_common_revs = 0 || Psq.is_empty state.rev_list then return None
    else
      match Psq.pop state.rev_list with
      | None -> return None
      | Some ((uid, (_, p, _)), rev_list) ->
          state.rev_list <- rev_list;
          parents uid store >>= fun ps ->
          p := !p lor _popped;
          if !p land _common = 0 then
            state.non_common_revs <- state.non_common_revs - 1;

          let mark = ref 0 in
          let res = ref (Some uid) in

          if !p land _common <> 0 then (
            mark := _common lor _seen;
            res := None )
          else if !p land _common_ref <> 0 then mark := _common lor _seen
          else mark := _seen;

          let rec loop = function
            | [] -> (
                match !res with None -> go () | Some _ as v -> return v )
            | (uid, p, ts) :: rest ->
                if !p land _seen = 0 then rev_list_push t (uid, p, ts) !mark;

                if !mark land _common <> 0 then
                  mark_common scheduler ~parents store t (uid, p, ts) true
                  >>= fun () -> loop rest
                else loop rest
          in
          loop ps
  in
  go ()

let next :
    type g s uid.
    s scheduler ->
    parents:(uid, g, s) parents ->
    (uid, uid * int ref * int64, g) store ->
    uid t ->
    (uid option, s) io =
 fun scheduler ~parents store t -> get_rev scheduler ~parents store t
