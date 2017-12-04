module type S =
sig
  module Hash: S.HASH
  module Inflate: S.INFLATE

  module Radix
    : module type of Radix.Make(struct include Hash let length _ = Hash.Digest.length end)

  module Graph: Map.S
    with type key = int64

  module PACKDecoder: Unpack.P
    with module Hash = Hash
     and module Inflate = Inflate

  type error =
    [ `Unexpected_end_of_input
    | `Unexpected_chunk of string
    | `PackDecoder of PACKDecoder.error ]

  val pp_error: error Fmt.t

  module Partial:
  sig
    type t =
      { hash  : Hash.t
      ; delta : (int64 * PACKDecoder.H.hunks) list }
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
    ; tree                    : (Crc32.t * Int64.t) Radix.t
    ; graph                   : (int * Hash.t option) Graph.t
    ; state                   : 'a }
  constraint 'a = [< state ]

  val v:
       ?max_length_object:int
    -> ?max_length_insert_hunks:int
    -> ?tree:(Crc32.t * int64) Radix.t
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
    (H: S.HASH)
    (I: S.INFLATE)
  : S with module Hash = H
       and module Inflate = I
= struct
  module Log =
  struct
    let src = Logs.Src.create "git.pack-info" ~doc:"logs git's pack-info event"
    include (val Logs.src_log src : Logs.LOG)
  end

  module Hash = H
  module Inflate = I

  module PACKDecoder = Unpack.MakePACKDecoder(Hash)(Inflate)
  module Radix
    : module type of Radix.Make(struct include Hash let length _ = Hash.Digest.length end)
    = Radix.Make(struct include Hash let length _ = Hash.Digest.length end)
  module Graph = Map.Make(Int64)

  type error =
    [ `Unexpected_end_of_input
    | `Unexpected_chunk of string
    | `PackDecoder of PACKDecoder.error ]

  let pp_error ppf = function
    | `Unexpected_end_of_input -> Fmt.pf ppf "`Unexpected_end_of_input"
    | `Unexpected_chunk chunk ->
      Fmt.pf ppf "(`Unexpected_chunk %a)"
        (Fmt.hvbox (Minienc.pp_scalar ~get:String.get ~length:String.length))
        chunk
    | `PackDecoder err -> Fmt.pf ppf "(`PackDecoder %a)" PACKDecoder.pp_error err

  module Partial =
  struct
    type t =
      { hash : Hash.t
      ; delta : (int64 * PACKDecoder.H.hunks) list }
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
    ; tree                    : (Crc32.t * Int64.t) Radix.t
    ; graph                   : (int * Hash.t option) Graph.t
    ; state                   : 'a }
    constraint 'a = [< state ]

  let option_default a v = match a with Some v -> v | None -> v

  let v ?max_length_object ?max_length_insert_hunks ?tree ?graph hash =
    { max_length_object = option_default max_length_object 0
    ; max_length_insert_hunks = option_default max_length_insert_hunks 0
    ; max_depth = 1
    ; tree = option_default tree Radix.empty
    ; graph = option_default graph Graph.empty
    ; state = `Empty hash }

  let from_stream ~ztmp ~window info stream =
    let state = PACKDecoder.default ztmp window in
    let empty = Cstruct.create 0 in

    let ctx_with_header chunk state =
      let ctx = Hash.Digest.init () in
      let hdr = Fmt.strf "%s %d\000"
          (match PACKDecoder.kind state with
           | PACKDecoder.Commit -> "commit"
           | PACKDecoder.Tag -> "tag"
           | PACKDecoder.Tree -> "tree"
           | PACKDecoder.Blob -> "blob"
           | _ -> assert false)
          (PACKDecoder.length state)
      in

      Hash.Digest.feed ctx (Cstruct.of_string hdr);
      Hash.Digest.feed ctx chunk;
      ctx
    in

    let open Lwt.Infix in

    let rec go ?(src = empty) ?ctx ?insert_hunks partial info state = match PACKDecoder.eval src state with
      | `Hunk (state, PACKDecoder.H.Insert raw) ->
        let insert_hunks = match insert_hunks with
          | Some count -> count + Cstruct.len raw
          | None -> Cstruct.len raw
        in

        go ~src ~insert_hunks partial info (PACKDecoder.continue state)
      | `Hunk (state, _) ->
        go ~src ?ctx ?insert_hunks partial info (PACKDecoder.continue state)
      | `Error (_, err) ->
        Lwt.return (Error (`PackDecoder err))
      | `Flush state ->
        let chunk, len = PACKDecoder.output state in

        let ctx = match ctx with
          | Some ctx -> Hash.Digest.feed ctx (Cstruct.sub chunk 0 len); ctx
          | None -> ctx_with_header (Cstruct.sub chunk 0 len) state
        in

        go ~src ~ctx partial info (PACKDecoder.flush 0 (Cstruct.len chunk) state)
      | `Object state ->
        let hash = match PACKDecoder.kind state, ctx with
          | (PACKDecoder.Commit
            | PACKDecoder.Tree
            | PACKDecoder.Tag
            | PACKDecoder.Blob), Some ctx -> Some (Hash.Digest.get ctx)
          | (PACKDecoder.Commit
            | PACKDecoder.Tree
            | PACKDecoder.Tag
            | PACKDecoder.Blob), None ->
            let ctx = ctx_with_header empty state in
            Some (Hash.Digest.get ctx)
          | _ -> None
        in

        let tree = match hash with
          | Some hash ->
            Radix.bind info.tree hash (PACKDecoder.crc state, PACKDecoder.offset state)
          | None -> info.tree
        in

        let max_length_insert_hunks = match PACKDecoder.kind state, insert_hunks with
          | PACKDecoder.Hunk _, Some insert_hunks ->
            max info.max_length_insert_hunks insert_hunks
          | PACKDecoder.Hunk _, None ->
            max info.max_length_insert_hunks 0
          | _ -> info.max_length_insert_hunks
        in

        let max_length_object = match PACKDecoder.kind state with
          | (PACKDecoder.Commit
            | PACKDecoder.Tree
            | PACKDecoder.Tag
            | PACKDecoder.Blob) ->
            max info.max_length_object (PACKDecoder.length state)
          | PACKDecoder.Hunk hunks_descr ->
            let max' = max hunks_descr.PACKDecoder.H.source_length hunks_descr.PACKDecoder.H.target_length in
            max info.max_length_object max'
        in

        let graph = match PACKDecoder.kind state with
          | PACKDecoder.Hunk ({ PACKDecoder.H.reference = PACKDecoder.H.Offset rel_off; _ }) ->
            let depth_source, _ =
              try Graph.find Int64.(sub (PACKDecoder.offset state) rel_off) info.graph
              with Not_found -> 0, None
            in

            Graph.add (PACKDecoder.offset state) (depth_source + 1, hash) info.graph
          | PACKDecoder.Hunk ({ PACKDecoder.H.reference = PACKDecoder.H.Hash hash; _ }) ->
            let depth_source, _ =
              try match Radix.lookup info.tree hash with
                | Some (_, abs_off) -> Graph.find abs_off info.graph
                | None -> 0, None
              with Not_found -> 0, None
            in

            Graph.add (PACKDecoder.offset state) (depth_source + 1, Some hash) info.graph
          | _ ->
            Graph.add (PACKDecoder.offset state) (0, hash) info.graph
        in

        let partial = match PACKDecoder.kind state with
          | PACKDecoder.Hunk hunks_descr ->
            (PACKDecoder.offset state, hunks_descr) :: partial
          | _ -> partial
        in

        go ~src partial
          { info with max_length_insert_hunks
                    ; max_length_object
                    ; tree
                    ; graph }
          (PACKDecoder.next_object state)
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
          go ~src ?ctx ?insert_hunks partial info (PACKDecoder.refill 0 (Cstruct.len src) state)
        | None ->
          Log.err (fun l -> l ~header:"from_stream" "Receive end of the PACK stream.");
          Lwt.return (Error `Unexpected_end_of_input)
    in

    go [] info state
end
