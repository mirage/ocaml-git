(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

let src = Logs.Src.create "negociator" ~doc:"logs negociator's event"

module Log = (val Logs.src_log src : Logs.LOG)

module Flag = struct
  type t = int

  let default = 0
  let complete = 1 lsl 0
  let common = 1 lsl 1
  let common_ref = 1 lsl 2
  let seen = 1 lsl 3
  let popped = 1 lsl 4
  let alternate = 1 lsl 5
  let is value x = x land value <> 0
  let _is_complete x = is x complete
  let is_common x = is x common
  let is_common_ref x = is x common_ref
  let is_seen x = is x seen
  let is_popped x = is x popped
  let _is_alternate x = is x alternate
  let _to_complete x = x lor complete
  let to_common x = x lor common
  let _to_common_ref x = x lor common_ref
  let to_seen x = x lor seen
  let to_popped x = x lor popped
  let _to_alternate x = x lor alternate

  let pp ppf x =
    let flags =
      [ (if is x complete then ["COMPLETE"] else [])
      ; (if is x common then ["COMMON"] else [])
      ; (if is x common_ref then ["COMMON_REF"] else [])
      ; (if is x seen then ["SEEN"] else [])
      ; (if is x popped then ["POPPED"] else [])
      ; (if is x alternate then ["ALTERNATE"] else []) ]
      |> List.concat
    in
    Fmt.list ~sep:(Fmt.unit " |@ ") Fmt.string ppf flags
end

(* XXX(dinosaure): see this paper
   https://github.com/ocamllabs/papers/blob/master/irmin/2014.08.matthieu/rapport.pdf *)

type 'a acks =
  { shallow: 'a list
  ; unshallow: 'a list
  ; acks: ('a * [`Common | `Ready | `Continue | `ACK]) list }

module type S = sig
  module Store : Minimal.S

  module Common :
    Smart.COMMON
    with type hash := Store.Hash.t
     and type reference := Store.Reference.t

  module Decoder :
    Smart.DECODER
    with module Hash := Store.Hash
     and module Reference := Store.Reference
     and module Common := Common

  type state
  type nonrec acks = Store.Hash.t acks

  val find_common :
       Store.t
    -> ( Store.Hash.Set.t
       * state
       * (   acks
          -> state
          -> ([`Again of Store.Hash.Set.t | `Done | `Ready] * state) Lwt.t) )
       Lwt.t
end

module Make (G : Minimal.S) = struct
  module Store = G

  module V = struct
    type t = {commit: Store.Value.Commit.t; mutable flags: Flag.t}

    let compare a b = Store.Value.Commit.compare b.commit a.commit

    let pp ppf {commit; flags} =
      Fmt.pf ppf "{ @[<hov>commit = %a;@ flags = [ %a ];@] }"
        (Fmt.hvbox Store.Value.Commit.pp)
        commit (Fmt.hvbox Flag.pp) flags
  end

  module Pq = Psq.Make (Store.Hash) (V)
  module Common = Smart.Common (Store.Hash) (Store.Reference)
  module Decoder = Smart.Decoder (Store.Hash) (Store.Reference) (Common)

  (* XXX(dinosaure): short-cut of the smart decoder module, the common way is
     to load the [Sync] module but, I don't want. And because we annotated some
     constraints about the type (and specifically about the hash), we can
     compile. *)

  module Bucket = struct
    type t = (Store.Hash.t, V.t) Hashtbl.t

    (* XXX(dinosaure): because the commit graph is a DAG, sometimes we ask to
       read a specific commit already computed. In the negotiation process, we
       keep some information for each commit (see {!Flag.t}) as a mutable
       value. So we keep a mutable « bucket » which contains all commit/value
       already processed. When, we ask to compute parents from a commit, we use
       this function to get the new commit from {!Store.read} or return the
       already computed commit with the mutable field flags kept.

       The flag is important to avoid to re-computed the same commit n-times
       (see the flag SEEN). *)

    let get t (bucket : t) hash =
      let open Lwt.Infix in
      try Hashtbl.find bucket hash |> fun v -> Lwt.return (Ok v)
      with Not_found -> (
        Store.read t hash
        >>= function
        | Ok (Store.Value.Commit commit) ->
            let value = {V.commit; flags= Flag.default} in
            Hashtbl.add bucket hash value ;
            Lwt.return (Ok value)
        | Ok (Store.Value.Tree _ | Store.Value.Tag _ | Store.Value.Blob _) ->
            Lwt.return (Error (`Invalid_hash hash))
        | Error err -> Lwt.return (Error (`Store err)) )
  end

  type rev = {pq: Pq.t; non_common_revs: int}

  let pp_rev ppf rev =
    Fmt.pf ppf "{ @[<hov>pq = %a;@ non_common_revs = %d;@] }"
      (Fmt.hvbox (Pq.pp Fmt.Dump.(pair Store.Hash.pp V.pp)))
      rev.pq rev.non_common_revs

  let push hash value mark rev =
    if (value.V.flags :> int) land mark = 0 then (
      value.V.flags <- (value.V.flags :> int) lor mark ;
      { pq= Pq.add hash value rev.pq
      ; non_common_revs=
          ( if not (Flag.is_common value.V.flags) then rev.non_common_revs + 1
          else rev.non_common_revs ) } )
    else rev

  (* XXX(dinosaure): this function marks a rev and its ancestors as common. In
     some cases, it is desirable to mark only the ancestors (for example, hen
     only the server does not yet know that they are common). *)
  let rec mark_common t bucket ~ancestors hash value rev =
    let open Lwt.Infix in
    if not (Flag.is_common value.V.flags) then (
      if not ancestors then value.V.flags <- Flag.to_common value.V.flags ;
      if not (Flag.is_seen value.V.flags) then
        push hash value Flag.seen rev |> fun rev -> Lwt.return rev
      else
        let rec go rev = function
          | [] -> Lwt.return rev
          | hash :: rest -> (
              Bucket.get t bucket hash
              >>= function
              | Error _ -> Lwt.return rev
              (* XXX(dinosaure): in git, if we can not get a commit from an
                 hash (for any reason), we just avoid the rest of the compute
                 (so we avoid the rest of the ancestors).

                 For me, we need to notice to the user the error (so avoid the
                 next compute and return only the error). So we need to think
                 about that if it's what we really want. TODO! *)
              | Ok value ->
                  mark_common t bucket ~ancestors:false hash value rev
                  >>= fun rev -> go rev rest )
        in
        let non_common_revs =
          if (not ancestors) && not (Flag.is_popped value.V.flags) then
            rev.non_common_revs - 1
          else rev.non_common_revs
        in
        go {rev with non_common_revs}
          (Store.Value.Commit.parents value.V.commit) )
    else Lwt.return rev

  let get t bucket rev =
    let open Lwt.Infix in
    let rec go rev =
      if rev.non_common_revs = 0 then Lwt.return (None, rev)
      else
        match Pq.pop rev.pq with
        | None -> Lwt.return (None, rev)
        | Some ((hash, ({V.commit; flags} as value)), pq) ->
            value.V.flags <- Flag.to_popped flags ;
            let non_common_revs =
              if not (Flag.is_common flags) then rev.non_common_revs - 1
              else rev.non_common_revs
            in
            let leave, marks =
              if Flag.is_common flags then
                false, Flag.to_common @@ Flag.to_seen @@ Flag.default
                (* XXX(dinosaure): do not send "have", and ignore ancestors. *)
              else if Flag.is_common_ref flags then
                true, Flag.to_common @@ Flag.to_seen @@ Flag.default
                (* XXX(dinosaure): send "have" and ignore ancestors. *)
              else true, Flag.to_seen @@ Flag.default
              (* XXX(dinosaure): send "have", also for its ancestors. *)
            in
            let rec gogo rev = function
              | [] -> Lwt.return rev
              | hash :: rest -> (
                  Bucket.get t bucket hash
                  >>= function
                  | Ok ({V.flags; _} as value) ->
                      let rev =
                        if not (Flag.is_seen flags) then
                          push hash value marks rev
                        else rev
                      in
                      ( if Flag.is_common marks then
                        mark_common t bucket ~ancestors:true hash value rev
                      else Lwt.return rev )
                      >>= fun rev -> gogo rev rest
                  | Error _ -> Lwt.return rev )
            in
            gogo {pq; non_common_revs} (Store.Value.Commit.parents commit)
            >>= fun rev ->
            if leave then Lwt.return (Some hash, rev) else go rev
    in
    go rev

  type state =
    { ready: bool (* got ready detail *)
    ; continue: bool (* got continue detail *)
    ; finish: bool (* want to finish *)
    ; count: int (* how many hashes we get *)
    ; flush: int (* how many hashes before flush *)
    ; vain: int (* how many vainly hashes we have *)
    ; rev: rev (* priority queue *)
    ; in_fly: Store.Hash.t list }

  type nonrec acks = Store.Hash.t acks

  let _pp_state ppf state =
    Fmt.pf ppf
      "{ @[<hov>ready = %b;@ continue = %b;@ finish = %b;@ count = %d;@ flush \
       = %d;@ vain = %d;@ rev = %a;@ in_fly = %a;@] }"
      state.ready state.continue state.finish state.count state.flush
      state.vain (Fmt.hvbox pp_rev) state.rev
      Fmt.Dump.(list Store.Hash.pp)
      state.in_fly

  (* XXX(dinosaure): to be clear, this implementation is very bad and we need
     to change it (TODO). For example, the [in_fly] field is used only one time
     (in the second call of [continue]. Then, we never used this field. You
     need to understand than this implementation is close to what git does -
     and apparently, it's not the best. *)

  exception Jump of Store.Hash.t list * state

  exception
    Invalid_commit of [`Store of Store.error | `Invalid_hash of Store.Hash.t]

  let _INITIAL_FLUSH = 16
  let _PIPESAFE_FLUSH = 32

  (* XXX(dinosaure): the canonical implementation will send up to 32 of 'have'
     at a time, then will send a flush packet line. The canonical
     implementation will skip ahead and send the next 32 immediately, so that
     there always a block of 32 "in-flight on the wire" at a time. *)
  let _VAIN = 128

  let update_flush count =
    if count < _PIPESAFE_FLUSH then count lsl 1 else count + _PIPESAFE_FLUSH

  let find_common t =
    let bucket = Hashtbl.create 1024 in
    let rev = {pq= Pq.empty; non_common_revs= 0} in
    let open Lwt.Infix in
    Store.Ref.list t
    >>= (fun refs ->
          Log.debug (fun l ->
              l "Local references: %a."
                (Fmt.hvbox
                   (Fmt.list (Fmt.pair Store.Reference.pp Store.Hash.pp)))
                refs ) ;
          Lwt_list.fold_left_s
            (fun rev (_, hash) ->
              Bucket.get t bucket hash
              >|= function
              | Ok value -> push hash value Flag.seen rev | Error _ -> rev )
            rev refs )
    >>= fun rev ->
    let rec consume have state = function
      | 0 -> Lwt.return (List.rev have, `Continue state)
      | n -> (
          get t bucket state.rev
          >>= function
          | None, rev ->
              Lwt.return (List.rev have, `Not_enough {state with rev})
          | Some hash, rev ->
              consume (hash :: have)
                {state with rev; count= state.count + 1; vain= state.vain + 1}
                (n - 1) )
    in
    let continue {acks; _} state =
      let rec go state have = function
        | [] -> Lwt.return (state, have)
        | (_, `ACK) :: _ ->
            (* XXX(dinosaure): without multi-ack or multi-ack-detailed,
               upload-pack sends 'ACK obj-id' on the first common object it
               finds. After that is says nothing until the client gives it a
               "done". *)
            Lwt.fail (Jump (have, state))
        | (hash, ((`Common | `Ready | `Continue) as ack)) :: rest -> (
            Bucket.get t bucket hash
            >>= function
            | Error err -> Lwt.fail (Invalid_commit err)
            | Ok value ->
                let vain, have' =
                  if ack = `Common && Flag.is_common value.V.flags then
                    0, hash :: have
                    (* XXX(dinosaure): we need to replay the have for this
                       object on the next RPC request so the peer knows it is
                       in common with us. *)
                  else if ack <> `Common then 0, have
                    (* XXX(dinosaure): reset [limit] because an ACK for this
                       commit has not been seen. *)
                  else state.vain + 1, have
                in
                mark_common t bucket ~ancestors:false hash value state.rev
                >|= ( if ack = `Ready then fun _ -> {rev with pq= Pq.empty}
                    else fun x -> x )
                >>= fun rev ->
                go
                  {state with rev; continue= true; ready= ack = `Ready; vain}
                  have' rest )
      in
      if state.finish && List.length state.in_fly = 0 then
        Lwt.return (`Done, state)
      else
        Lwt.try_bind
          (fun () -> go {state with in_fly= []} state.in_fly acks)
          (fun (state, have) ->
            (* XXX(dinosaure): this test does not appear on the Git code base
               but it's the default case to end up the negotiation engine. *)
            if state.ready then Lwt.return (`Ready, state)
            else if state.continue && state.vain > _VAIN then
              Lwt.return
                ( `Again (Store.Hash.Set.of_list have)
                , {state with finish= true} )
            else
              let rec go state have =
                (* XXX(dinosaure): consume to the [state.flush] limit. *)
                get t bucket state.rev
                >>= function
                | Some hash, rev ->
                    let state =
                      { state with
                        vain= state.vain + 1; count= state.count + 1; rev }
                    in
                    if state.count >= state.flush then
                      Lwt.return
                        ( `Again (Store.Hash.Set.of_list (hash :: have))
                        , {state with flush= update_flush state.count} )
                    else go state (hash :: have)
                | None, rev ->
                    Lwt.return
                      ( `Again (Store.Hash.Set.of_list have)
                      , {state with rev; finish= true} )
                (* XXX(dinosaure): in the next step, we stop. *)
              in
              go state have )
          (function
            | Jump (have, state) ->
                (* XXX(dinosaure): if we received a ready flag, that means is
                   not necessary to continue the negotiation (the server found
                   an acceptable common base commit) and we will receive then
                   the PACK file even if we don't write "done". *)
                if not state.ready then (
                  Log.debug (fun l ->
                      l ~header:"find_common"
                        "We catch a jump exception to get out of the main \
                         loop and return `Again." ) ;
                  Lwt.return
                    ( `Again (Store.Hash.Set.of_list have)
                    , {state with finish= true} ) )
                else (
                  Log.debug (fun l ->
                      l ~header:"find_common"
                        "We catch a jump exception to get out of the main \
                         loop and return `Ready." ) ;
                  Lwt.return (`Ready, state) )
            | exn -> Lwt.fail exn)
      (* XXX(dinosaure): you need to take care about the exception
         [Invalid_commit]. This can be happens when the server ACK a wrong
         commit (that means the client does not have this commit). In this
         case, the better case is to abort all.

         NOTE: if you read the code, you can notice that we silent any error
         from the store (for example, when we try to get an object from the
         store and catch an error, we continue the compute without any warning
         or exception). *)
    in
    (* XXX(dinosaure): first, we consume [_INITIAL_FLUSH] hashes of the
       priority queue. *)
    consume []
      { ready= false
      ; continue= false
      ; finish= false
      ; count= 0
      ; flush= _INITIAL_FLUSH
      ; vain= 0
      ; rev
      ; in_fly= [] }
      _INITIAL_FLUSH
    >>= function
    | have, `Not_enough state ->
        Lwt.return
          ( Store.Hash.Set.of_list have
          , {state with finish= true}
          , fun _ state -> Lwt.return (`Done, {state with finish= true}) )
    | have, `Continue state -> (
        (* XXX(dinosaure): then, we update the flush limit: in other words,
           which time we send the next have list. *)
        let state = {state with flush= update_flush state.count} in
        (* XXX(dinosaure): we keep one window "ahead" of the other side, and
           will wait for an ACK only on the next one. *)
        consume [] state state.flush
        >>= function
        | _, `Not_enough state ->
            Lwt.return
              ( Store.Hash.Set.of_list have
              , {state with finish= true}
              , fun _ state -> Lwt.return (`Done, {state with finish= true}) )
        | in_fly, `Continue state ->
            Lwt.return
              (Store.Hash.Set.of_list have, {state with in_fly}, continue) )
end
