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

module type S =
sig
  module Hash: S.HASH
  module Inflate: S.INFLATE

  module HDec: Unpack.H with module Hash := Hash
  module PDec: Unpack.P
    with module Hash := Hash
     and module Inflate := Inflate
     and module Hunk := HDec

  type error =
    [ `Unexpected_end_of_input
    | `Unexpected_chunk of string
    | `PDec of PDec.error ]

  val pp_error: error Fmt.t

  type delta =
    | Unresolved of { hash: Hash.t; length: int; }
    | Internal of { hash: Hash.t; abs_off: int64; length: int; }
    | Delta of { hunks_descr: HDec.hunks; inserts: int; depth: int; from: delta; }

  val pp_delta: delta Fmt.t

  type path = Load of int | Patch of { hunks: int; target: int; src: path; }

  type 'a t =
    { index : (Hash.t, Crc32.t * int64 * int) Hashtbl.t
    ; delta : (int64, delta) Hashtbl.t
    ; hash_pack : Hash.t
    ; state : 'a }
  constraint 'a = [< `Pass | `Normalized of path | `Resolved of path ]

  val v: Hash.t -> [ `Pass ] t
  val normalize: length:int -> [ `Pass ] t -> [ `Normalized of path ] t
  val resolve: length:int -> [ `Normalized of path ] t -> [ `Resolved of path ] t

  val first_pass:
       ztmp:Cstruct.t
    -> window:Inflate.window
    -> ?idx:(Hash.t -> (Crc32.t * int64) option)
    -> (unit -> Cstruct.t option Lwt.t)
    -> ([ `Normalized of path ] t, error) result Lwt.t
end

module Make
    (Hash: S.HASH)
    (Inflate: S.INFLATE)
    (HDec: Unpack.H with module Hash := Hash)
    (PDec: Unpack.P with module Hash := Hash
                     and module Inflate := Inflate
                     and module Hunk := HDec)
  : S with module Hash = Hash
       and module Inflate = Inflate
       and module HDec := HDec
       and module PDec := PDec
= struct
  let src = Logs.Src.create "git.pack-info" ~doc:"logs git's pack-info event"
  module Log = (val Logs.src_log src : Logs.LOG)

  module Hash = Hash
  module Inflate = Inflate
  module HDec = HDec
  module PDec = PDec

  type error =
    [ `Unexpected_end_of_input
    | `Unexpected_chunk of string
    | `PDec of PDec.error ]

  let pp_error ppf = function
    | `Unexpected_end_of_input ->
      Fmt.pf ppf "Unexpected end of PACK stream"
    | `Unexpected_chunk chunk ->
      Fmt.pf ppf "Unexpected chunk of PACK stream: %a"
        (Fmt.hvbox (Encore.Lole.pp_scalar ~get:String.get ~length:String.length))
        chunk
    | `PDec err ->
      Fmt.pf ppf "Got an error while decoding PACK stream: %a" PDec.pp_error err

  type path = Load of int | Patch of { hunks: int; target: int; src: path; }

  type delta =
    | Unresolved of { hash: Hash.t; length: int; }
    | Internal   of { hash: Hash.t; abs_off: int64; length: int; }
    | Delta      of { hunks_descr: HDec.hunks; inserts: int; depth: int; from: delta; }

  let rec pp_delta ppf = function
    | Unresolved { hash; length; } ->
      Fmt.pf ppf "(Unresolved@ { @[<hov>hash = %a;@ \
                  length = %d;@] })"
        Hash.pp hash length
    | Internal { hash; abs_off; length; } ->
      Fmt.pf ppf "(Internal { @[<hov>hash = %al@ \
                  abs_off = %Ld;@ \
                  length = %d;@] })"
        Hash.pp hash abs_off length
    | Delta { hunks_descr; inserts; depth; from; } ->
      Fmt.pf ppf "(Delta { @[<hov>hunks_descr = %a;@ \
                  inserts = %d;@ \
                  depth = %d;@ \
                  delta = %a;@] })"
        HDec.pp_hunks hunks_descr
        inserts depth pp_delta from

  type 'a t =
    { index : (Hash.t, Crc32.t * int64 * int) Hashtbl.t
    ; delta : (int64, delta) Hashtbl.t
    ; hash_pack : Hash.t
    ; state : 'a }
    constraint 'a = [< `Pass | `Normalized of path | `Resolved of path ]

  let v hash_pack =
    { index = Hashtbl.create 128
    ; delta = Hashtbl.create 128
    ; hash_pack
    ; state = `Pass }

  let rec merge abs_off path acc = match path, acc with
    | Unresolved { length; _ }, Load x
    | Internal { length; _ },   Load x -> Load (max length x)
    | Unresolved { length; _ }, Patch { hunks; target; src; }
    | Internal { length; _ },   Patch { hunks; target; src; } -> Patch { hunks; target = max target length; src; }
    | Delta { hunks_descr = { HDec.source_length
                            ; target_length
                            ;  _ }
            ; inserts; _ },
      Load x ->
      Patch { hunks = inserts; target = max x target_length; src = Load source_length }
    | Delta { hunks_descr = { HDec.reference = HDec.Offset rel_off
                            ; target_length
                            ; _ }
            ; inserts; from; _ },
      Patch { hunks; target; src; } ->
      let abs_off = Int64.sub abs_off rel_off in
      let src = merge abs_off from src in
      Patch { hunks = max hunks inserts; target = max target target_length; src; }
    | Delta { hunks_descr = { HDec.reference = HDec.Hash _
                            ; target_length; _ }
            ; inserts; from; _ },
      Patch { hunks; target; src; } ->
      let src = merge 0L from src in
      (* XXX(dinosaure): because source is unresolved, we cannot know [abs_off]
         but it does not matter for merging. *)
      Patch { hunks = max hunks inserts; target = max target target_length; src; }

  let normalize paths = Hashtbl.fold merge paths (Load 0)

  let first_pass ~ztmp ~window ?(idx = (fun _hash -> None)) stream =
    let state = PDec.default ztmp window in
    let empty = Cstruct.create 0 in
    let index = Hashtbl.create 128 in (* hash: crc32, absolute offset *)
    let delta = Hashtbl.create 128 in

    let ctx_with_header chunk state =
      let ctx = Hash.Digest.init () in
      let hdr = Fmt.strf "%s %d\000"
          (match PDec.kind state with
           | PDec.Commit -> "commit"
           | PDec.Tag -> "tag"
           | PDec.Tree -> "tree"
           | PDec.Blob -> "blob"
           | _ -> assert false)
          (PDec.length state) in

      let ctx = Hash.Digest.feed ctx (Cstruct.of_string hdr) in
      let ctx = Hash.Digest.feed ctx chunk in
      ctx in

    let open Lwt.Infix in

    let rec go ?(src = empty) ?ctx ?insert_hunks state = match PDec.eval src state with
      | `Hunk (state, HDec.Insert raw) ->
        let insert_hunks = match insert_hunks with
          | Some count -> count + Cstruct.len raw
          | None -> Cstruct.len raw in

        go ~src ~insert_hunks (PDec.continue state)
      | `Hunk (state, _) ->
        go ~src ?ctx ?insert_hunks (PDec.continue state)
      | `Error (_, err) ->
        Lwt.return (Error (`PDec err))
      | `Flush state ->
        let chunk, len = PDec.output state in

        let ctx = match ctx with
          | Some ctx -> Hash.Digest.feed ctx (Cstruct.sub chunk 0 len)
          | None -> ctx_with_header (Cstruct.sub chunk 0 len) state in

        go ~src ~ctx (PDec.flush 0 (Cstruct.len chunk) state)
      | `Object state ->
        let () = match PDec.kind state with
          | PDec.Hunk ({ HDec.reference = HDec.Offset rel_off; _ } as hunks_descr) ->
            let abs_off = Int64.(sub (PDec.offset state) rel_off) in
            let inserts = match insert_hunks with Some x -> x | None -> 0 in

            let from =
              try Hashtbl.find delta abs_off
              with Not_found -> invalid_arg "invalid pack stream" in
            (* XXX(dinosaure): if we can not find source from absolute offset,
               that means pack stream is invalid - offset points to previous (already saved) entry in any case. *)
            let depth_source = match from with Delta { depth; _ } -> depth | _ -> 0 in
            Hashtbl.add delta (PDec.offset state) (Delta { hunks_descr; inserts; depth = depth_source + 1; from; })
          | PDec.Hunk ({ HDec.reference = HDec.Hash hash; source_length; _ } as hunks_descr) ->
            let inserts = match insert_hunks with Some x -> x | None -> 0 in

            let from =
              try let _, abs_off = match idx hash with
                  | Some x -> x
                  | None -> Hashtbl.find index hash |> fun (crc, abs_off, _) -> (crc, abs_off) in
                Hashtbl.find delta abs_off
              with Not_found -> Unresolved { hash; length = source_length; } in
            let depth_source = match from with Delta { depth; _ } -> depth | _ -> 0 in
            Hashtbl.add delta (PDec.offset state) (Delta { hunks_descr; inserts; depth = depth_source + 1; from; })
          | _ -> match ctx with
            | Some ctx ->
              let hash = Hash.Digest.get ctx in

              Log.info (fun l -> l ~header:"first_pass" "Save object %a." Hash.pp hash);

              Hashtbl.add index hash (PDec.crc state, PDec.offset state, PDec.length state);
              Hashtbl.add delta (PDec.offset state) (Internal { hash; abs_off = PDec.offset state; length = PDec.length state; })
            | None ->
              let ctx = ctx_with_header empty state in
              let hash = Hash.Digest.get ctx in

              Log.info (fun l -> l ~header:"first_pass" "Save object %a." Hash.pp hash);

              Hashtbl.add index (Hash.Digest.get ctx) (PDec.crc state, PDec.offset state, PDec.length state);
              Hashtbl.add delta (PDec.offset state) (Internal { hash; abs_off = PDec.offset state; length = PDec.length state; }) in

        go ~src (PDec.next_object state)
      | `End (_, hash_pack) ->
        (stream () >>= function
          | Some raw ->
            Log.err (fun l -> l ~header:"first_pass" "Expected end of pack stream but retrieve: %a."
                        (Fmt.hvbox (Encore.Lole.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len))
                        raw);
            Lwt.return (Error (`Unexpected_chunk (Cstruct.to_string raw)))
          | None ->
            Log.debug (fun l -> l ~header:"first_pass" "End of the PACK stream.");

            Lwt.return (Ok { index; delta; hash_pack; state = `Normalized (normalize delta) }))
      | `Await state ->
        Log.debug (fun l -> l ~header:"first_pass" "Waiting more input.");

        stream () >>= function
        | Some src ->
          Log.debug (fun l -> l ~header:"first_pass" "Receive a chunk of the PACK stream (length: %d)."
                        (Cstruct.len src));
          go ~src ?ctx ?insert_hunks (PDec.refill 0 (Cstruct.len src) state)
        | None ->
          Log.err (fun l -> l ~header:"first_pass" "Receive end of the PACK stream.");
          Lwt.return (Error `Unexpected_end_of_input) in

    go state

  let resolve ~length t =
    if Hashtbl.length t.index = length
    then { t with state = `Resolved (normalize t.delta) }
    else invalid_arg "promote: invalid argument"

  let normalize ~length t =
    if Hashtbl.length t.delta = length
    then { t with state = `Normalized (normalize t.delta) }
    else invalid_arg "promote: invalid argument"
end
