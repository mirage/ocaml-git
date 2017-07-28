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

module type C =
sig
  val default : Smart.capability list
end

module type NET =
sig
  type socket

  val read   : socket -> Bytes.t -> int -> int -> int Lwt.t
  val write  : socket -> Bytes.t -> int -> int -> int Lwt.t
  val socket : string -> int -> socket Lwt.t
  val close  : socket -> unit Lwt.t
end

module Make
    (Net : NET)
    (Store : Store.S with type Digest.t = Bytes.t
                      and type Digest.buffer = Cstruct.t
                      and type Hash.t = Bytes.t (* Digest.t: FIXME! *)
                      and type FileSystem.File.error = [ `System of string ]
                      and type FileSystem.File.raw = Cstruct.t
                      and type FileSystem.Dir.error = [ `System of string ]
                      and type FileSystem.Mapper.error = [ `System of string ]
                      and type FileSystem.Mapper.raw = Cstruct.t)
    (Capabilities : C)
= struct
  module Client = Smart.Client(Store.Digest)
  module Digest = Store.Digest
  module Inflate = Store.Inflate
  module Deflate = Store.Deflate
  module Path = Store.Path
  module Revision = Revision.Make(Store)

  module Hash =
  struct
    include Store.Digest

    let compare = Helper.BaseBytes.compare
    let equal   = Helper.BaseBytes.equal
    let pp      = Helper.BaseBytes.pp
    let hash    = Helper.BaseBytes.hash

    let of_string x = Bytes.unsafe_of_string x
    let to_string x = Bytes.unsafe_to_string x

    let to_hex_string x = Helper.BaseBytes.to_hex x
    let of_hex_string x = Helper.BaseBytes.of_hex x
  end

  module PACKDecoder = Unpack.MakePACKDecoder(Hash)(Inflate)
  module PACKEncoder = Pack.MakePACKEncoder(Hash)(Deflate)
  module IDXEncoder  = Index_pack.Encoder(Hash)
  module Decoder     = Unpack.MakeDecoder(Hash)(Store.FileSystem.Mapper)(Inflate)
  module Tree        : module type of Radix.Make(Bytes) = Radix.Make(Bytes)

  type error =
    [ `SmartPack of string
    | `Pack      of PACKEncoder.error
    | `Unpack    of PACKDecoder.error
    | `Decoder   of Decoder.error
    | `Clone     of string
    | `Ls        of string
    | `Push      of string
    | `Idx       of IDXEncoder.error
    | `Not_found
    | Store.FileSystem.File.error (* | Store.FileSystem.Dir.error | Store.FileSystem.Mapper.error *) ]

  let pp_error ppf = function
    | `SmartPack err -> Helper.ppe ~name:"`SmartPack" Fmt.string ppf err
    | `Unpack err    -> Helper.ppe ~name:"`Unpack" (Fmt.hvbox PACKDecoder.pp_error) ppf err
    | `Pack err      -> Helper.ppe ~name:"`Pack" (Fmt.hvbox PACKEncoder.pp_error) ppf err
    | `Decoder err   -> Helper.ppe ~name:"`Decoder" (Fmt.hvbox Decoder.pp_error) ppf err
    | `Clone err     -> Helper.ppe ~name:"`Clone" Fmt.string ppf err
    | `Push err      -> Helper.ppe ~name:"`Push" Fmt.string ppf err
    | `Ls err        -> Helper.ppe ~name:"`Ls" Fmt.string ppf err
    | `Not_found     -> Fmt.string ppf "`Not_found"
    | `Idx err       -> Helper.ppe ~name:"`Idx" (Fmt.hvbox IDXEncoder.pp_error) ppf err
    | #Store.FileSystem.File.error as err -> Store.FileSystem.File.pp_error ppf err

  type t =
    { socket : Net.socket
    ; input  : Bytes.t
    ; output : Bytes.t
    ; ctx    : Client.context }

  let err_unexpected_result result =
    let buf = Buffer.create 64 in
    let ppf = Fmt.with_buffer buf in

    Fmt.pf ppf "Unexpected result: %a%!" (Fmt.hvbox Client.pp_result) result;
    Buffer.contents buf

  let rec process t result =
    let open Lwt.Infix in

    match result with
    | `Read (buffer, off, len, continue) ->
      Net.read t.socket t.input 0 len >>= fun n ->
      Cstruct.blit_from_bytes t.input 0 buffer off len;
      process t (continue n)
    | `Write (buffer, off, len, continue) ->
      Cstruct.blit_to_bytes buffer off t.output 0 len;
      Net.write t.socket t.output 0 len >>= fun n ->
      process t (continue n)
    | `Error (err, buf, committed) ->
      assert false (* TODO *)
    | #Client.result as result -> Lwt.return result

  let option_map f = function
    | Some v -> Some (f v)
    | None -> None

  let packer ~window ~depth git ~ofs_delta remote commands =
    let open Lwt.Infix in

    let commands' =
      (List.map (fun (hash, refname, _) -> Client.Encoder.Delete (hash, refname)) remote
      @ commands)
    in

    (* XXX(dinosaure): we don't want to delete remote references but we want to
      exclude any commit already stored remotely. So, we « delete » remote
      references from the result set. *)

    Lwt_list.fold_left_s
      (fun acc -> function
          | Client.Encoder.Create _ -> Lwt.return acc
          | Client.Encoder.Update (hash, _, _) ->
            Revision.(Range.normalize git (Range.Include (from_hash hash)))
            >|= Store.Hash.Set.union acc
          | Client.Encoder.Delete (hash, _) ->
            Revision.(Range.normalize git (Range.Include (from_hash hash)))
            >|= Store.Hash.Set.union acc)
      Store.Hash.Set.empty commands'
    >>= fun negative ->
    Lwt_list.fold_left_s
      (fun acc -> function
        | Client.Encoder.Create (hash, _) ->
          Revision.(Range.normalize git (Range.Include (from_hash hash)))
          >|= Store.Hash.Set.union acc
        | Client.Encoder.Update (_, hash, _) ->
          Revision.(Range.normalize git (Range.Include (from_hash hash)))
          >|= Store.Hash.Set.union acc
        | Client.Encoder.Delete _ -> Lwt.return acc)
      Store.Hash.Set.empty commands'
    >|= (fun positive -> Revision.Range.E.diff positive negative)
    >>= fun elements ->
    Lwt_list.fold_left_s
      (fun acc commit ->
        Format.printf "send commit: %a\n%!" Store.Hash.pp commit;

        Store.fold git
          (fun (acc, max_length) ?name ~length hash -> function
              | Store.Value.Commit commit ->
                PACKEncoder.Entry.make hash
                  Pack.Kind.Commit
                  length
                |> fun entry ->
                Store.Hash.Map.add hash entry acc
                |> fun acc -> Lwt.return (acc, max max_length length)
              | Store.Value.Tree tree ->
                PACKEncoder.Entry.make hash
                  ?name:(option_map Path.to_string name)
                  Pack.Kind.Tree
                  length
                |> fun entry ->
                Store.Hash.Map.add hash entry acc
                |> fun acc -> Lwt.return (acc, max max_length length)
              | Store.Value.Blob blob ->
                PACKEncoder.Entry.make hash
                  ?name:(option_map Path.to_string name)
                  Pack.Kind.Blob
                  length
                |> fun entry ->
                Store.Hash.Map.add hash entry acc
                |> fun acc -> Lwt.return (acc, max max_length length)
              | Store.Value.Tag tag ->
                PACKEncoder.Entry.make hash
                  Pack.Kind.Tag
                  length
                |> fun entry ->
                Store.Hash.Map.add hash entry acc
                |> fun acc -> Lwt.return (acc, max max_length length))
          ~path:(Path.v "/") acc commit)
      (Store.Hash.Map.empty, 0L) (Store.Hash.Set.elements elements)
    >>= fun (entries, max) ->
    PACKEncoder.Delta.deltas
      ~memory:false
      (Store.Hash.Map.bindings entries |> List.map snd)
      (fun hash -> Store.raw git hash >|= function Some (_, raw) -> Some raw | None -> None)
      (fun _ -> false) (* TODO *)
      window depth
    >|= function
    | Ok lst ->
      let htmp = Cstruct.create 0x8000 in
      Ok (PACKEncoder.default htmp lst)
    | Error (PACKEncoder.Delta.Invalid_hash hash) -> Error (`Pack (PACKEncoder.Invalid_hash hash))

  module Pack =
  struct
    let random_string len =
      let gen () = match Random.int (26 + 26 + 10) with
        | n when n < 26 -> int_of_char 'a' + n
        | n when n < 26 + 26 -> int_of_char 'A' + n - 26
        | n -> int_of_char '0' + n - 26 - 26
      in
      let gen () = char_of_int (gen ()) in

      Bytes.create len |> fun raw ->
      for i = 0 to len - 1 do Bytes.set raw i (gen ()) done;
      Bytes.unsafe_to_string raw

    let pack_filename () =
      Fmt.strf "pack-%s.pack" (random_string 10)

    module Graph = Map.Make(Int64)

    let string_of_kind = function
      | PACKDecoder.Commit -> "commit"
      | PACKDecoder.Tree   -> "tree"
      | PACKDecoder.Blob   -> "blob"
      | PACKDecoder.Tag    -> "tag"
      | _ -> raise (Invalid_argument "string_of_kind")

    (* XXX(dinosaure): I explain this big code. This explanation includes
       [normalize_tree]. Firstly, about the Smart protocol:

       - the Smart module takes care about the side-band, so if the capabilities
       does not mention the side-band or the side-band-64K, [`PACK (`Out _)] and
       [`PACK (`Err _)] should never appear). Then, the [raw] is already cleaned
       (you can manipulate [raw] as it is).

       - secondly, about the computation of the stream. As you know, the
       PACKDecoder is a non-blocking decoder of the PACK file. We start a first
       pass to decode the PACK file received. So, it's easy by the API provided by
       PACKDecoder and the semantic of the PACKDecoder state.

       In this pass, we try to collect hash, CRC-32 check-sum and offset for each
       non-delta-ified objects. This case appears in [`Flush]. At same time, we
       calculate the dependency-graph for the delta-ified objects. This
       computation is very close to an heuristic of the PACK file: the base of
       the object is always before the object (in the writing order).

       So, we populate a tree (to store the binding [hash -> (crc, offset)]),
       which contains partially what the PACK contains (only non-delta-ified
       object) and lists, which contain all delta-ified objects (with
       meta-data: crc and offset).

       Then, with the dependency-graph, we can calculate the biggest depth of
       the delta-ification. In real world, this value ca not be upper than 50.

       We can start the second pass [normalize_tree]. It consists to construct
       all delta-ified object to compute the hash and complete the partial tree.
       Because we know the deepest object, we can allocate what is really needed
       to construct all object of this specific PACK file.

       So, we use the complex function [optimized_get'] from the Decoder module
       which does not allocate any buffer in the major heap. This last point is
       very important because for a large PACK file, you can be close to the
       [Out_of_memory] problem.

       Finally, we generate from the complete tree the IDX file and it's done!

       I'm joking. Indeed, when we fetch to a repository, we can receive a
       _thin-pack_ (that means some references can be extern of the PACK file).
       The previous compute retains its validity but we store a _thin-pack_ in
       the store. It seems that git does not do the same and recompute a new
       PACK file from the _thin-pack_ which one is canonical (that means all
       reference can be found in the PACK file).

       We talked about lists for delta-ified objects. Indeed, we define 2 lists,
       one contains all delta-ified object with an internal reference
       ({!PACKEncoder.H.Offset}). The second list contains all other delta-ified
       objects which the reference is external.

       Then, for the first list, we follow the same delta-ification provided by
       the server (found in the _thin-pack_). We don't try to recalculate the
       best delta-ification for these objects. We agglomerate the external
       object then and mark as free to try any delta-ification.

       We compute finally the delta-ification. The [get] function is very
       special because the _thin-pack_ stay on the [temp] directory. So any
       object from this _thin-pack_ is not available from the store. Otherwise,
       for the external object, by the negotiation engine than we can believe it is
       available.

       In the end, we encode the new PACK file with [safe_encoder_to_file] and
       continue to use the previous buffers allocated in [normalize_tree] to get
       any object.

       NOTE: we have different way to optimize in memory and in computation this
       last process. But consider this process as slow. Read a _thin-pack_,
       populate a tree, process a new PACK file and encode it is slow. And it's
       not mandatory to do this process as git because we can store a
       _thin-pack_ directly and ocaml-git or git can handle that.

       TODO:

       - instead to re-generate the Rabin's fingerprint between 2 objects,
       which the delta-ification is provided by the server, we can only
       copy/paste the raw from the _thin-pack_ to the new PACK file and just
       change the relative offset stored in the object's header.

    *)
    type pack_state =
      { pack  : PACKDecoder.t
      ; tree  : (Crc32.t * int64) Tree.t
      ; hash  : Hash.ctx option
      ; rofs  : (PACKDecoder.H.hunks * Crc32.t * int64) list
      ; rext  : (PACKDecoder.H.hunks * Crc32.t * int64) list
      ; graph : int Graph.t
      ; max_length : int }

    let make_pack ztmp wtmp =
      { pack = PACKDecoder.default ztmp wtmp
      ; tree = Tree.empty
      ; hash = None
      ; rofs = []
      ; rext = []
      ; graph = Graph.empty
      ; max_length = 0 }

    let rec pack_handler k c t w r =
      let open Lwt.Infix in

      match r with
      | `PACK (`Out raw) ->
        Client.run c.ctx `ReceivePACK |> process c >>= pack_handler k c t w
      | `PACK (`Err raw) ->
        let err = Cstruct.to_string raw in
        Lwt.return (Error (`SmartPack err))
      | `PACK `End ->

        let max_depth = Graph.fold (fun off depth acc -> max depth acc) t.graph 0 in

        (match PACKDecoder.eval (Cstruct.create 0) t.pack with
         | `End (pack, hash_pack) ->
           Store.FileSystem.File.close w >>= (function
               | Ok () -> k t.max_length max_depth hash_pack t.tree t.rofs t.rext
               | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err))
         | _ -> Lwt.return (Error (`SmartPack "bad end state")))
      | `PACK (`Raw raw) ->
        Store.FileSystem.File.write raw w >>=
        (function
          | Error (#Store.FileSystem.File.error as err) ->
            Lwt.return (Error err)
          | Ok n ->
            let rec go t = match PACKDecoder.eval raw t.pack with
              | `Await pack ->
                Ok { t with pack; }
              | `End (pack, hash) ->
                Ok { t with pack = PACKDecoder.refill 0 0 pack; }
              | `Error (pack, err) -> Error (`Unpack err)
              | `Flush pack ->
                let o, n = PACKDecoder.output pack in

                let hash = match t.hash with
                  | Some ctx ->
                    if n > 0 then Hash.feed ctx (Cstruct.sub o 0 n);
                    Some ctx
                  | None ->
                    let hdr_kind = match PACKDecoder.kind pack with
                      | (PACKDecoder.Commit
                        | PACKDecoder.Blob
                        | PACKDecoder.Tag
                        | PACKDecoder.Tree) as kind -> string_of_kind kind
                      | _ -> assert false
                    in

                    let hdr = Fmt.strf "%s %Ld\000" hdr_kind (Int64.of_int (PACKDecoder.length pack)) in
                    let ctx = Hash.init () in

                    Hash.feed ctx (Cstruct.of_string hdr);
                    if n > 0 then Hash.feed ctx (Cstruct.sub o 0 n);

                    Some ctx
                in

                go { t with hash; pack = PACKDecoder.flush 0 (Cstruct.len o) pack }
              | `Hunk (pack, _) ->
                go { t with pack = PACKDecoder.continue pack }
              | `Object pack ->
                let crc = PACKDecoder.crc pack in
                let off = PACKDecoder.offset pack in

                let t = match PACKDecoder.kind pack, t.hash with
                  | (PACKDecoder.Commit
                    | PACKDecoder.Tree
                    | PACKDecoder.Tag
                    | PACKDecoder.Blob), Some ctx ->

                    let hash = Hash.get ctx in

                    { t with pack = PACKDecoder.next_object pack
                           ; hash = None
                           ; tree = Tree.bind t.tree hash (crc, off)
                           ; max_length = max t.max_length (PACKDecoder.length pack) }
                  | PACKDecoder.Hunk ({ PACKDecoder.H.reference = PACKDecoder.H.Offset rel_off; _ } as hunks), None ->
                    let graph =
                      let depth_base =
                        try Graph.find Int64.(sub off rel_off) t.graph
                        with Not_found -> 0
                      in

                      Graph.add off (depth_base + 1) t.graph
                    in

                    { t with pack = PACKDecoder.next_object pack
                           ; rofs = (hunks, crc, off) :: t.rofs
                           ; graph
                           ; max_length = max t.max_length
                               @@ max (PACKDecoder.length pack)
                               @@ max hunks.PACKDecoder.H.target_length hunks.PACKDecoder.H.source_length }
                  | PACKDecoder.Hunk hunks, None ->
                    { t with pack = PACKDecoder.next_object pack
                           ; rext = (hunks, crc, off) :: t.rext
                           ; graph = Graph.add off 1 t.graph
                           ; max_length = max t.max_length
                               @@ max (PACKDecoder.length pack)
                               @@ max hunks.PACKDecoder.H.target_length hunks.PACKDecoder.H.source_length }

                  | (PACKDecoder.Commit
                    | PACKDecoder.Tree
                    | PACKDecoder.Tag
                    | PACKDecoder.Blob) as kind, None ->
                    let hdr = Fmt.strf "%s 0\000" (string_of_kind kind) in
                    let ctx = Hash.init () in

                    assert (PACKDecoder.length pack = 0);
                    Hash.feed ctx (Cstruct.of_string hdr);

                    let hash = Hash.get ctx in

                    { t with pack = PACKDecoder.next_object pack
                           ; hash = None
                           ; tree = Tree.bind t.tree hash (crc, off) }
                  | PACKDecoder.Hunk _, Some _ -> assert false
                in

                go t
            in

            (* XXX(dinosaure): we can compress [raw] and recall [go] and avoid
               the error - this is the same problem than
               [Helper.safe_encoder_to_file] but I'm lazy to handle for this
               moment that. TODO! *)

            if n = Cstruct.len raw
            then match go { t with pack = PACKDecoder.refill 0 (Cstruct.len raw) t.pack } with
              | Ok t -> Client.run c.ctx `ReceivePACK |> process c >>= pack_handler k c t w
              | Error (`Unpack err) -> Lwt.return (Error (`Unpack err))
            else Lwt.return (Error (`System "Loose something when we write the PACK file received")))
      | result -> Lwt.return (Error (`Clone (err_unexpected_result result)))

    let hash_of_object o =
      let ctx = Hash.init () in

      let hdr_kind = match o.Decoder.Object.kind with
        | `Commit -> "commit"
        | `Blob   -> "blob"
        | `Tree   -> "tree"
        | `Tag    -> "tag"
      in

      let hdr = Fmt.strf "%s %Ld\000" hdr_kind o.Decoder.Object.length in

      Hash.feed ctx (Cstruct.of_string hdr);
      if Cstruct.len o.Decoder.Object.raw > 0 then Hash.feed ctx o.Decoder.Object.raw;

      Hash.get ctx

    exception Leave of Decoder.error

    type info =
      { tree       : (Crc32.t * int64) Tree.t
      ; hash       : Hash.t
      ; rofs       : (PACKDecoder.H.hunks * Crc32.t * int64) list
      ; rext       : (PACKDecoder.H.hunks * Crc32.t * int64) list
      ; graph      : Hash.t Graph.t
      ; max_length : int (* XXX(dinosaure): we can remove this information available in [rtmp]. *)
      ; max_depth  : int
      ; decoder    : Decoder.t
      ; fd         : Store.FileSystem.Mapper.fd
      ; ztmp       : Cstruct.t
      ; htmp       : Cstruct.t array
      ; rtmp       : Cstruct.t * Cstruct.t * int
      ; wtmp       : Inflate.window }

    let rec normalize_tree git pack_filename max_length max_depth hash_pack partial_tree rofs rext =
      let open Lwt.Infix in

      let tree' = ref partial_tree in

      Store.FileSystem.Mapper.openfile pack_filename
      >>= function
      | Error (#Store.FileSystem.Mapper.error as err) -> Lwt.return (Error err)
      | Ok fd ->
        Decoder.make fd
            (fun hash -> None)
            (fun hash -> Tree.lookup !tree' hash)
            (fun offset -> None)
            (extern git)
        >>= function
        | Ok decoder ->
          let rtmp = (Cstruct.create max_length, Cstruct.create max_length, max_length) in
          let htmp =
            Cstruct.create (max_length * (max_depth + 1))
            |> fun raw -> Array.init (max_depth + 1) (fun i -> Cstruct.sub raw (i * max_length) max_length)
          in
          let ztmp = Store.buffer_zl git in
          let wtmp = Store.buffer_window git in

          let max = List.length (rofs @ rext) in

          Lwt.try_bind
            (fun () -> Lwt_list.fold_left_s
                (fun (n, graph) (hunks, crc, offset) ->
                   Decoder.optimized_get' ~h_tmp:htmp decoder offset rtmp ztmp wtmp >>= function
                   | Ok o ->
                     let hash = hash_of_object o in

                     Format.printf "\rResolving delta: %d%% (%d/%d)." (n * 100 / max) n max;

                     tree' := Tree.bind !tree' hash (crc, offset);
                     Lwt.return (n + 1, Graph.add offset hash graph)
                   | Error err -> Lwt.fail (Leave err))
                (0, Graph.empty) (rofs @ rext))
            (fun (n, graph) -> Lwt.return (Ok { tree = !tree'
                                              ; hash = hash_pack
                                              ; rofs
                                              ; rext
                                              ; graph
                                              ; max_length
                                              ; max_depth
                                              ; decoder
                                              ; fd
                                              ; ztmp
                                              ; htmp
                                              ; rtmp
                                              ; wtmp }))
            (function Leave err -> Lwt.return (Error (`Decoder err)))
          >>= fun res ->
          Format.printf "\rResolving delta: 100%% (%d/%d), done.\n%!" max max;

          (* Store.FileSystem.Mapper.close fd *) Lwt.return (Ok ()) >>=
          (function Ok () -> Lwt.return res
                  | Error (#Store.FileSystem.Mapper.error as err) -> Lwt.return (Error err))
        | Error (#Store.FileSystem.Mapper.error as err) -> Lwt.return (Error err)
    and extern git hash =
      Store.raw git hash

    let canonicalize_pack git info =
      let open Lwt.Infix in

      let k2k = function
        | `Commit -> Pack.Kind.Commit
        | `Blob -> Pack.Kind.Blob
        | `Tree -> Pack.Kind.Tree
        | `Tag -> Pack.Kind.Tag
      in

      let make acc (hash, (crc, off)) =
        Format.printf "hash: %a\n%!" Hash.pp hash;

        Decoder.optimized_get' ~h_tmp:info.htmp info.decoder off info.rtmp info.ztmp info.wtmp >>= function
        | Error err ->
          Lwt.fail (Leave err)
        | Ok o ->
          let delta = match o.Decoder.Object.from with
            | Decoder.Object.External hash -> Some (PACKEncoder.Entry.From hash)
            | Decoder.Object.Direct _ -> None
            | Decoder.Object.Offset { offset; _ } ->
              try Some (PACKEncoder.Entry.From (Graph.find offset info.graph))
              with Not_found -> None
          in

          Lwt.return (PACKEncoder.Entry.make hash ?delta (k2k o.Decoder.Object.kind) o.Decoder.Object.length :: acc)
      in

      let ext = List.fold_left (fun acc -> function
          | ({ PACKDecoder.H.reference = PACKDecoder.H.Hash hash; _ }, _, _) ->
            (match Tree.lookup info.tree hash with
             | Some _ -> acc (* XXX(dinosaure): available in the thin-pack. *)
             | None ->
               try List.find (Hash.equal hash) acc |> fun _ -> acc (* XXX(dinosaure): avoid duplicate. *)
               with Not_found -> hash :: acc)
          | _ -> assert false)
          []
          info.rext
      in

      let plus acc =
        Lwt_list.fold_left_s
          (fun acc hash ->
             Store.raw_p
              ~ztmp:(Store.buffer_zl git)
              ~dtmp:(Store.buffer_de git)
              ~raw:(Store.buffer_io git)
              ~window:(Store.buffer_window git)
              git hash
            >>= function
            | Some (kind, raw) ->
              Lwt.return (PACKEncoder.Entry.make hash (k2k kind) (Int64.of_int (Cstruct.len raw)) :: acc)
            | None ->
              Lwt.fail (Leave (Decoder.Invalid_hash hash)))
          acc ext
      in

      let get hash =
        if Tree.exists info.tree hash
        then Decoder.get_with_allocation ~h_tmp:info.htmp info.decoder hash info.ztmp info.wtmp >>= function
          | Error err ->
            Lwt.return None
          | Ok o -> Lwt.return (Some o.Decoder.Object.raw)
        else Store.raw git hash >>= function
          | Some (_, raw) -> Lwt.return (Some raw)
          | None -> Lwt.return None
      in

      let tag hash = false in

      Tree.to_list info.tree |> Lwt_list.fold_left_s make [] >>= plus >>= fun entries ->
      PACKEncoder.Delta.deltas ~memory:false entries get tag 10 50 >>= function
      | Error (PACKEncoder.Delta.Invalid_hash hash) ->
        Lwt.fail (Leave (Decoder.Invalid_hash hash))
      | Ok entries ->
        let ztmp  = Cstruct.create 0x8000 in
        let state = PACKEncoder.default ztmp entries in

        let module E =
        struct
          type state  = { pack : PACKEncoder.t
                        ; src  : Cstruct.t option }

          type raw    = Cstruct.t
          type result = { tree : (Crc32.t * int64) Tree.t
                        ; hash : Hash.t }
          type error  = PACKEncoder.error

          let raw_length = Cstruct.len
          let raw_blit   = Cstruct.blit

          let option_value ~default = function
            | Some v -> v
            | None -> default

          let empty = Cstruct.create 0

          type await' = [ `Await of state ]
          type rest' = [ `Flush of state | `End of (state * result) | `Error of (state * error) ]

          let rec eval dst state =
            match PACKEncoder.eval (option_value ~default:empty state.src) dst state.pack with
            | `End (pack, hash) ->
              Lwt.return (`End ({ state with pack; }, { tree = (PACKEncoder.idx pack)
                                                      ; hash }))
            | `Error (pack, err) ->
              Lwt.return (`Error ({ state with pack; }, err))
            | `Flush pack ->
              Lwt.return (`Flush { state with pack; })
            | `Await pack ->
              match state.src with
              | Some _ -> eval dst { pack = PACKEncoder.finish pack
                                   ; src  = None }
              | None ->
                let hash = PACKEncoder.expect pack in

                (if Tree.exists info.tree hash
                 then Decoder.optimized_get ~h_tmp:info.htmp info.decoder hash info.rtmp info.ztmp info.wtmp >>= function
                   | Error err -> Lwt.fail (Leave err)
                   | Ok o -> Lwt.return o.Decoder.Object.raw
                 else Store.raw git hash >>= function
                   | Some (_, raw) -> Lwt.return raw
                   | None -> Lwt.fail (Leave (Decoder.Invalid_hash hash)))
                >>= fun raw -> eval dst { pack = PACKEncoder.refill 0 (Cstruct.len raw) pack
                                        ; src  = Some raw }


          let flush off len ({ pack; _ } as state) = { state with pack = PACKEncoder.flush off len pack }
          let used { pack; _ } = PACKEncoder.used_out pack
        end in

        let new_pack_filename = pack_filename () in

        Store.FileSystem.Dir.temp () >>= fun temp ->

        let abs_new_pack_filename = Store.Path.(temp / new_pack_filename) in

        Store.FileSystem.File.open_w abs_new_pack_filename ~mode:0o644 ~lock:(Lwt.return ()) >>= function
        | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err)
        | Ok fd ->
          let input = Cstruct.create 0x8000 in

          Helper.safe_encoder_to_file
            ~limit:50
            (module E)
            Store.FileSystem.File.write
            fd input { E.src = None; pack = state; }
          >>= function
          | Ok { E.tree; E.hash; } -> Store.FileSystem.File.close fd >>= (function
              | Ok () -> Lwt.return (Ok (abs_new_pack_filename, tree, hash))
              | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err))
          | Error err ->
            Store.FileSystem.File.close fd >>= function
            | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err)
            | Ok () -> match err with
              | `Stack -> Lwt.return (Error (`System "Impossible to store the PACK file"))
              | `Writer (#Store.FileSystem.File.error as err) -> Lwt.return (Error err)
              | `Encoder err -> Lwt.return (Error (`Pack err))
  end

  module Idx =
  struct
    let save git = function
      | Error err -> Lwt.return (Error err)
      | Ok (filename, tree, hash) ->
        let open Lwt.Infix in

        let pack_obj = Store.Path.(Store.dotgit git / "objects" / "pack" / (Fmt.strf "pack-%a.pack" Hash.pp hash)) in
        Store.FileSystem.File.move filename pack_obj >>= function
        | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err)
        | Ok () ->
          let idx = Store.Path.(Store.dotgit git / "objects" / "pack" / (Fmt.strf "pack-%a.idx" Hash.pp hash)) in

          Store.FileSystem.File.open_w idx ~mode:0o644 ~lock:(Lwt.return ()) >>= function
          | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err)
          | Ok fd ->
            let state = IDXEncoder.default (Tree.to_sequence tree) hash in
            let input = Store.buffer_de git in

            let module E =
            struct
              type state  = IDXEncoder.t
              type raw    = Cstruct.t
              type result = unit
              type error  = IDXEncoder.error

              let raw_length = Cstruct.len
              let raw_blit   = Cstruct.blit

              type end' = [ `End of state ]
              type rest = [ `Flush of state | `Error of state * error ]

              let eval raw state = match IDXEncoder.eval raw state with
                | #end' as v -> let `End state = v in Lwt.return (`End (state, ()))
                | #rest as res -> Lwt.return res

              let flush = IDXEncoder.flush
              let used = IDXEncoder.used_out
            end in

            Helper.safe_encoder_to_file
              ~limit:50
              (module E)
              Store.FileSystem.File.write
              fd input state
            >>= function
            | Ok () -> Store.FileSystem.File.close fd >>= (function
                | Ok () -> Lwt.return (Ok ())
                | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err))
            | Error err ->
              Store.FileSystem.File.close fd >>= function
              | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err)
              | Ok () -> match err with
                | `Stack -> Lwt.return (Error (`System "Impossible to store the IDX file"))
                | `Writer (#Store.FileSystem.File.error as err) -> Lwt.return (Error err)
                | `Encoder err -> Lwt.return (Error (`Idx err))
  end

  let rec clone_handler git t r =
    let open Lwt.Infix in

    match r with
    | `Negociation l ->
      Client.run t.ctx `Done
      |> process t
      >>= clone_handler git t
    | `NegociationResult status ->
      let ztmp = Store.buffer_zl git in
      let wtmp = Store.buffer_window git in
      let pack_filename = Pack.pack_filename () in

      Store.FileSystem.Dir.temp () >>= fun temp ->

      let abs_pack_filename = Store.Path.(temp / pack_filename) in

      Store.FileSystem.File.open_w ~lock:(Lwt.return ()) ~mode:0o644
        abs_pack_filename
      >>= (function
          | Ok w ->
            Client.run t.ctx `ReceivePACK
            |> process t
            >>= Pack.pack_handler
              (Pack.normalize_tree git abs_pack_filename) t (Pack.make_pack ztmp wtmp) w
               >>= (function
                   | Ok info -> Idx.save git (Ok (abs_pack_filename, info.Pack.tree, info.Pack.hash))
                   | Error _ as err ->  Idx.save git err)
          | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err))
    | `ShallowUpdate shallow_update ->
      Client.run t.ctx (`Has []) |> process t >>= clone_handler git t
    | `Refs refs ->
      (try
         let (hash_head, head, peeled) =
           List.find
             (fun (_, refname, peeled) -> refname = "HEAD" && not peeled)
             refs.Client.Decoder.refs
         in
         Client.run t.ctx (`UploadRequest { Client.Encoder.want = hash_head, [ hash_head ]
                                          ; capabilities = Capabilities.default
                                          ; shallow = []
                                          ; deep = None })
         |> process t
         >>= clone_handler git t
       with Not_found ->
         Client.run t.ctx `Flush
         |> process t
         >>= function `Flush -> Lwt.return (Error `Not_found)
                    | result -> Lwt.return (Error (`Clone (err_unexpected_result result))))
    | result -> Lwt.return (Error (`Clone (err_unexpected_result result)))

  let ls_handler git t r =
    let open Lwt.Infix in

    match r with
    | `Refs refs ->
      Client.run t.ctx `Flush
      |> process t
      >>= (function `Flush -> Lwt.return (Ok refs.Client.Decoder.refs)
                  | result -> Lwt.return (Error (`Ls (err_unexpected_result result))))
    | result -> Lwt.return (Error (`Ls (err_unexpected_result result)))

  let fetch_handler git ?(shallow = []) ~notify ~negociate:(fn, state) ~has ~want ?deepen ~thin t r =
    let open Lwt.Infix in

    let pack ~thin t =
      let ztmp = Store.buffer_zl git in
      let wtmp = Store.buffer_window git in
      let pack_filename = Pack.pack_filename () in

      Store.FileSystem.Dir.temp () >>= fun temp ->

      let abs_pack_filename = Store.Path.(temp / pack_filename) in

      Store.FileSystem.File.open_w ~lock:(Lwt.return ()) ~mode:0o644 abs_pack_filename
      >>= (function
          | Ok w ->
            Client.run t.ctx `ReceivePACK
            |> process t
            >>= Pack.pack_handler
              (Pack.normalize_tree git Store.Path.(temp / pack_filename))
              t (Pack.make_pack ztmp wtmp) w
            >>= (function
                | Ok info ->
                  (* XXX(dinosaure): we take care about [info.fd] and close it
                     when we don't need to use [info.decoder] - this is happen
                     only before [Idx.save], [Pack.canonicalize_pack] needs
                     [info.decoder]. *)

                  (if thin
                   then Store.FileSystem.Mapper.close info.Pack.fd >>= function
                     | Ok () -> Idx.save git (Ok (abs_pack_filename, info.Pack.tree, info.Pack.hash))
                     | Error #Store.FileSystem.Mapper.error as err -> Lwt.return err
                   else Pack.canonicalize_pack git info >>= function
                     | Error _ as err ->
                       Store.FileSystem.Mapper.close info.Pack.fd >>= fun _ -> Lwt.return err
                     | Ok _ as value -> Store.FileSystem.Mapper.close info.Pack.fd >>= function
                       | Ok () -> Idx.save git value
                       | Error #Store.FileSystem.Mapper.error as err -> Lwt.return err)
                | Error _ as err -> Lwt.return err)
          | Error (#Store.FileSystem.File.error as err) -> Lwt.return (Error err))
    in

    let rec aux t state = function
      | `ShallowUpdate shallow_update ->
        notify shallow_update >>= fun () ->
        Client.run t.ctx (`Has has) |> process t >>= aux t state
      | `Negociation acks ->
        fn acks state >>=
        (function
          | `Ready, state -> pack ~thin t
          | `Done, state ->
            Client.run t.ctx `Done |> process t >>= aux t state
          | `Again has, state ->
            Client.run t.ctx (`Has has) |> process t >>= aux t state)
      | `NegociationResult status -> pack ~thin t
      | `Refs refs ->
        want refs.Client.Decoder.refs >>=
        (function
          | first :: rest ->
            Client.run t.ctx (`UploadRequest { Client.Encoder.want = first, rest
                                             ; capabilities = Capabilities.default
                                             ; shallow
                                             ; deep = deepen })
            |> process t
            >>= aux t state
          | [] -> Client.run t.ctx `Flush
                  |> process t
            >>= (function `Flush -> Lwt.return (Ok ())
                        | result -> Lwt.return (Error (`Fetch (err_unexpected_result result)))))
      | result -> Lwt.return (Error (`Ls (err_unexpected_result result)))
    in

    aux t state r

  let push_handler git ~push ~packer t r =
    let open Lwt.Infix in

    let empty = Cstruct.create 0 in
    let option_value ~default = function Some v -> v | None -> default in

    let rec pack src state t r =
      let rec go src dst state t =
        match PACKEncoder.eval (option_value ~default:empty src) dst state with
        | `Flush state -> Lwt.return (Ok (`Continue (state, src)))
        | `End (state, hash) ->
          (if PACKEncoder.used_out state = 0
            then Lwt.return (Ok `Finish)
            else Lwt.return (Ok (`Continue (state, src))))
        | `Error (state, err) -> Lwt.return (Error (`Pack err))
        | `Await state ->
          (match src with
            | Some _ ->
              Lwt.return (Ok (None, PACKEncoder.finish state))
            | None ->
              let hash = PACKEncoder.expect state in

              Store.raw git hash >>= function
              | Some (_, raw) -> Lwt.return (Ok (Some raw, PACKEncoder.refill 0 (Cstruct.len raw) state))
              | None -> Lwt.return (Error (`Pack (PACKEncoder.Invalid_hash hash))))
          >>= function Ok (src, state) -> go src dst state t
                      | Error _ as err -> Lwt.return err
      in

      match r with
      | `ReadyPACK dst ->
        go src dst (PACKEncoder.flush 0 (Cstruct.len dst) state) t >>= (function
            | Ok (`Continue (state, src)) ->
              Client.run t.ctx (`SendPACK (PACKEncoder.used_out state))
              |> process t
              >>= pack src state t
            | Ok `Finish ->
              Client.run t.ctx `FinishPACK
              |> process t
              >>= pack src state t
            | Error (`Pack _ as err) -> Lwt.return (Error err))
      | result -> Lwt.return (Error (`Push (err_unexpected_result result)))
    in

    let rec aux t refs commands = function
      | `Refs refs ->
        let capabilities =
          List.filter (function
              | `Report_status | `Delete_refs | `Ofs_delta | `Push_options | `Agent _ | `Side_band | `Side_band_64k -> true
              | _ -> false)
            Capabilities.default
        in

        (push git refs.Client.Decoder.refs >>= function
          | (_,  []) ->
            Client.run t.ctx `Flush
            |> process t
            >>= (function `Flush -> Lwt.return (Ok ())
                        | result -> Lwt.return (Error (`Push (err_unexpected_result result))))
          | (shallow, (x :: r as commands)) ->
            Client.run t.ctx (`UpdateRequest { Client.Encoder.shallow
                                             ; requests = Client.Encoder.L (x, r)
                                             ; capabilities })
            |> process t
            >>= aux t (Some refs.Client.Decoder.refs) (Some commands))
      | `ReadyPACK _ as result ->
        let ofs_delta = List.exists ((=) `Ofs_delta) (Client.capabilities t.ctx) in
        let commands = match commands with Some commands -> commands | None -> assert false in
        let refs     = match refs with Some refs -> refs | None -> assert false in

        (* XXX(dinosaure): in this case, we can use GADT to describe the
           protocol by the session-type (like, [`UpdateRequest] makes a
           [`] response). So, we can constraint some assertions about
           the context when we catch [`ReadyPACK].

           One of this assertion is about the [commands] variable, which one is
           previously specified. So, the [None] value can not be catch and it's
           why we have an [assert false]. *)

        packer git ~ofs_delta refs commands >>= (function
          | Ok state -> pack None state t result
          | Error _ as err -> Lwt.return err)
      | result -> Lwt.return (Error (`Push (err_unexpected_result result)))
    in

    aux t None None r

  let push git ~push ~packer ?(port = 9418) host path =
    let open Lwt.Infix in

    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `ReceivePack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx }
    in
    process t state >>= push_handler git ~push ~packer t >>= fun v -> Net.close socket >>= fun () -> Lwt.return v

  let ls git ?(port = 9418) host path =
    let open Lwt.Infix in

    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx }
    in
    process t state >>= ls_handler git t >>= fun v -> Net.close socket >>= fun () -> Lwt.return v

  let fetch git ?(shallow = []) ~notify ~negociate ~has ~want ?deepen ~thin ?(port = 9418) host path =
    let open Lwt.Infix in

    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx }
    in
    process t state
    >>= fetch_handler git ~shallow ~notify ~negociate ~has ~want ?deepen ~thin t
    >>= fun v -> Net.close socket
    >>= fun () -> Lwt.return v

  let clone git ?(port = 9418) host path =
    let open Lwt.Infix in

    Net.socket host port >>= fun socket ->
    let ctx, state = Client.context { Client.Encoder.pathname = path
                                    ; host = Some (host, Some port)
                                    ; request_command = `UploadPack }
    in
    let t = { socket
            ; input = Bytes.create 65535
            ; output = Bytes.create 65535
            ; ctx }
    in
    process t state >>= clone_handler git t >>= fun v -> Net.close socket >>= fun () -> Lwt.return v
end
