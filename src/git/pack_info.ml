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
  module Graph: Map.S with type key = int64
  module Map: Map.S with type key = Hash.t

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

  module Partial:
  sig
    type t =
      { hash  : Hash.t
      ; delta : (int64 * HDec.hunks) list }
  end

  module Full:
  sig
    type t =
      { hash : Hash.t
      ; thin : bool }
  end

  type partial = [ `Partial of Partial.t ]
  and empty    = [ `Empty of Hash.t ]
  and full     = [ `Full of Full.t ]

  type state =
    [ empty
    | partial
    | full ]

  type 'a t =
    { max_length_object       : int
    ; max_length_insert_hunks : int
    ; max_depth               : int
    ; tree                    : (Crc32.t * Int64.t) Map.t
    ; graph                   : (int * Hash.t option) Graph.t
    ; state                   : 'a }
  constraint 'a = [< state ]

  val v:
       ?max_length_object:int
    -> ?max_length_insert_hunks:int
    -> ?tree:(Crc32.t * int64) Map.t
    -> ?graph:(int * Hash.t option) Graph.t
    -> Hash.t -> [> empty ] t

  val from_stream:
       ztmp:Cstruct.t
    -> window:Inflate.window
    -> [> empty ] t
    -> (unit -> Cstruct.t option Lwt.t)
    -> (partial t, error) result Lwt.t
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
  module Log =
  struct
    let src = Logs.Src.create "git.pack-info" ~doc:"logs git's pack-info event"
    include (val Logs.src_log src : Logs.LOG)
  end

  module Hash = Hash
  module Inflate = Inflate
  module HDec = HDec
  module PDec = PDec

  module Graph = Map.Make(Int64)
  module Map = Map.Make(Hash)

  type error =
    [ `Unexpected_end_of_input
    | `Unexpected_chunk of string
    | `PDec of PDec.error ]

  let pp_error ppf = function
    | `Unexpected_end_of_input ->
      Fmt.pf ppf "Unexpected end of PACK stream"
    | `Unexpected_chunk chunk ->
      Fmt.pf ppf "Unexpected chunk of PACK stream: %a"
        (Fmt.hvbox (Minienc.pp_scalar ~get:String.get ~length:String.length))
        chunk
    | `PDec err ->
      Fmt.pf ppf "Got an error while decoding PACK stream: %a" PDec.pp_error err

  module Partial =
  struct
    type t =
      { hash : Hash.t
      ; delta : (int64 * HDec.hunks) list }
  end

  module Full =
  struct
    type t =
      { hash : Hash.t
      ; thin : bool }
  end

  type partial = [ `Partial of Partial.t ]
  and empty    = [ `Empty of Hash.t ]
  and full     = [ `Full of Full.t ]

  type state =
    [ empty
    | partial
    | full ]

  type 'a t =
    { max_length_object       : int
    ; max_length_insert_hunks : int
    ; max_depth               : int
    ; tree                    : (Crc32.t * Int64.t) Map.t
    ; graph                   : (int * Hash.t option) Graph.t
    ; state                   : 'a }
    constraint 'a = [< state ]

  let option_default a v = match a with Some v -> v | None -> v

  let v ?max_length_object ?max_length_insert_hunks ?tree ?graph hash =
    { max_length_object = option_default max_length_object 0
    ; max_length_insert_hunks = option_default max_length_insert_hunks 0
    ; max_depth = 1
    ; tree = option_default tree Map.empty
    ; graph = option_default graph Graph.empty
    ; state = `Empty hash }

  let from_stream ~ztmp ~window info stream =
    let state = PDec.default ztmp window in
    let empty = Cstruct.create 0 in

    let ctx_with_header chunk state =
      let ctx = Hash.Digest.init () in
      let hdr = Fmt.strf "%s %d\000"
          (match PDec.kind state with
           | PDec.Commit -> "commit"
           | PDec.Tag -> "tag"
           | PDec.Tree -> "tree"
           | PDec.Blob -> "blob"
           | _ -> assert false)
          (PDec.length state)
      in

      Hash.Digest.feed ctx (Cstruct.of_string hdr);
      Hash.Digest.feed ctx chunk;
      ctx
    in

    let open Lwt.Infix in

    let rec go ?(src = empty) ?ctx ?insert_hunks partial info state = match PDec.eval src state with
      | `Hunk (state, HDec.Insert raw) ->
        let insert_hunks = match insert_hunks with
          | Some count -> count + Cstruct.len raw
          | None -> Cstruct.len raw
        in

        go ~src ~insert_hunks partial info (PDec.continue state)
      | `Hunk (state, _) ->
        go ~src ?ctx ?insert_hunks partial info (PDec.continue state)
      | `Error (_, err) ->
        Lwt.return (Error (`PDec err))
      | `Flush state ->
        let chunk, len = PDec.output state in

        let ctx = match ctx with
          | Some ctx -> Hash.Digest.feed ctx (Cstruct.sub chunk 0 len); ctx
          | None -> ctx_with_header (Cstruct.sub chunk 0 len) state
        in

        go ~src ~ctx partial info (PDec.flush 0 (Cstruct.len chunk) state)
      | `Object state ->
        let hash = match PDec.kind state, ctx with
          | (PDec.Commit
            | PDec.Tree
            | PDec.Tag
            | PDec.Blob), Some ctx -> Some (Hash.Digest.get ctx)
          | (PDec.Commit
            | PDec.Tree
            | PDec.Tag
            | PDec.Blob), None ->
            let ctx = ctx_with_header empty state in
            Some (Hash.Digest.get ctx)
          | _ -> None
        in

        let tree = match hash with
          | Some hash ->
            Map.add hash (PDec.crc state, PDec.offset state) info.tree
          | None -> info.tree
        in

        let max_length_insert_hunks = match PDec.kind state, insert_hunks with
          | PDec.Hunk _, Some insert_hunks ->
            max info.max_length_insert_hunks insert_hunks
          | PDec.Hunk _, None ->
            max info.max_length_insert_hunks 0
          | _ -> info.max_length_insert_hunks
        in

        let max_length_object = match PDec.kind state with
          | (PDec.Commit
            | PDec.Tree
            | PDec.Tag
            | PDec.Blob) ->
            max info.max_length_object (PDec.length state)
          | PDec.Hunk hunks_descr ->
            let max' = max
                hunks_descr.HDec.source_length
                hunks_descr.HDec.target_length in
            max info.max_length_object max'
        in

        let graph = match PDec.kind state with
          | PDec.Hunk ({ HDec.reference = HDec.Offset rel_off; _ }) ->
            let depth_source, _ =
              try Graph.find Int64.(sub (PDec.offset state) rel_off) info.graph
              with Not_found -> 0, None
            in

            Graph.add (PDec.offset state) (depth_source + 1, hash) info.graph
          | PDec.Hunk ({ HDec.reference = HDec.Hash hash; _ }) ->
            let depth_source, _ =
              try let _, abs_off = Map.find hash info.tree in
                  Graph.find abs_off info.graph
              with Not_found -> 0, None
            in

            Graph.add (PDec.offset state) (depth_source + 1, Some hash) info.graph
          | _ ->
            Graph.add (PDec.offset state) (0, hash) info.graph
        in

        let partial = match PDec.kind state with
          | PDec.Hunk hunks_descr ->
            (PDec.offset state, hunks_descr) :: partial
          | _ -> partial
        in

        go ~src partial
          { info with max_length_insert_hunks
                    ; max_length_object
                    ; tree
                    ; graph }
          (PDec.next_object state)
      | `End (_, hash) ->
        (stream () >>= function
          | Some raw ->
            Log.err (fun l -> l ~header:"from_stream" "Expected end of pack stream but retrieve: %a."
                        (Fmt.hvbox (Minienc.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len))
                        raw);
            Lwt.return (Error (`Unexpected_chunk (Cstruct.to_string raw)))
          | None ->
            Log.debug (fun l -> l ~header:"from_stream" "End of the PACK stream.");

            Lwt.return
              (Ok { info with state = `Partial { Partial.hash; delta = List.rev partial; }
                            (* XXX(dinosaure): [List.rev] is very
                               important to keep the topological
                               order for any next computation of
                               this pack file. *)
                            ; max_depth = Graph.fold (fun _ (depth, _) acc -> max depth acc) info.graph 1 }))
      | `Await state ->
        Log.debug (fun l -> l ~header:"from_stream" "Waiting more input.");

        stream () >>= function
        | Some src ->
          Log.debug (fun l -> l ~header:"from_stream" "Receive a chunk of the PACK stream (length: %d)."
                        (Cstruct.len src));
          go ~src ?ctx ?insert_hunks partial info (PDec.refill 0 (Cstruct.len src) state)
        | None ->
          Log.err (fun l -> l ~header:"from_stream" "Receive end of the PACK stream.");
          Lwt.return (Error `Unexpected_end_of_input)
    in

    go [] info state
end
