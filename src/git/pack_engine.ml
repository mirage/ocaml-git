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

open Lwt.Infix
let ( >>!= ) a f = Lwt_result.bind_lwt_err a f
let ( >>?= ) = Lwt_result.bind
let ( >>|= ) = Lwt_result.map
let ( >!= ) a f = Lwt_result.map_err f a

let src = Logs.Src.create "git.pack" ~doc:"Git pack engine"
module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig

  module Hash: S.HASH
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module FS: S.FS

  module HDec: Unpack.H with module Hash := Hash
  module PDec: Unpack.P
    with module Hash := Hash
     and module Inflate := Inflate
     and module Hunk := HDec
  module RPDec: Unpack.D
    with module Hash := Hash
     and type Mapper.fd = FS.Mapper.fd
     and type Mapper.error = FS.error
     and module Inflate := Inflate
     and module Hunk := HDec
     and module Pack := PDec

  module PEnc: Pack.P
    with module Hash = Hash
     and module Deflate = Deflate

  module IDec: Index_pack.LAZY
    with module Hash = Hash

  module IEnc: Index_pack.ENCODER
    with module Hash = Hash

  module PInfo: Pack_info.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module HDec := HDec
     and module PDec := PDec

  type t
  type loaded

  type error =
    [ `Pack_decoder of RPDec.error
    | `Pack_encoder of PEnc.error
    | `Pack_info    of PInfo.error
    | `Idx_decoder  of IDec.error
    | `Idx_encoder  of IEnc.error
    | FS.error Error.FS.t
    | `Invalid_hash of Hash.t
    | `Integrity    of string
    | `Not_found ]

  val pp_error: error Fmt.t

  val v: FS.t -> Fpath.t list -> t Lwt.t

  val add_total:
       root:Fpath.t
    -> t
    -> Fpath.t
    -> PInfo.full PInfo.t
    -> (Hash.t * int, error) result Lwt.t

  val add_exists:
       root:Fpath.t
    -> t
    -> Hash.t
    -> (unit, error) result Lwt.t

  val merge: t -> [> PInfo.partial | PInfo.full ] PInfo.t -> unit Lwt.t

  val load_index:
       FS.t -> Fpath.t
    -> (Hash.t * IDec.t * FS.Mapper.fd, Fpath.t * error) result Lwt.t

  val load_partial:
       root:Fpath.t
    -> t
    -> Hash.t
    -> IDec.t
    -> FS.Mapper.fd
    -> (loaded, error) result Lwt.t

  val force_total:
       t
    -> loaded
    -> ((RPDec.t * FS.Mapper.fd * PInfo.full PInfo.t), error) result Lwt.t

  val lookup: t -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t

  val mem: t -> Hash.t -> bool Lwt.t

  val list: t -> Hash.t list Lwt.t

  val read:
       root:Fpath.t
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> t
    -> Hash.t
    -> (RPDec.Object.t, error) result Lwt.t

  val size:
       root:Fpath.t
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> t
    -> Hash.t
    -> (int, error) result Lwt.t

  val save_idx_file:
       fs:FS.t -> root:Fpath.t
    -> (Hash.t * (Crc32.t * int64)) IEnc.sequence
    -> Hash.t
    -> (unit, error) result Lwt.t

  val save_pack_file:
       fs:FS.t -> (string -> string)
    -> (PEnc.Entry.t * PEnc.Delta.t) list
    -> (Hash.t -> Cstruct.t option Lwt.t)
    -> (Fpath.t * (Hash.t * (Crc32.t * int64)) IEnc.sequence * Hash.t, error) result Lwt.t
end

module Make
    (Hash: S.HASH)
    (FS: S.FS)
    (Inflate: S.INFLATE)
    (Deflate: S.DEFLATE)
    (HDec: Unpack.H with module Hash := Hash)
    (PDec: Unpack.P with module Hash := Hash
                     and module Inflate := Inflate
                     and module Hunk := HDec)
    (RPDec: Unpack.D with module Hash := Hash
                      and type Mapper.fd = FS.Mapper.fd
                      and type Mapper.error = FS.error
                      and module Inflate := Inflate
                      and module Hunk := HDec
                      and module Pack := PDec)
  : S with module Hash = Hash
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module FS = FS
       and module HDec := HDec
       and module PDec := PDec
       and module RPDec := RPDec
= struct

  module Hash = Hash
  module Inflate = Inflate
  module Deflate = Deflate
  module FS = Helper.FS(FS)

  module HDec = HDec
  module PDec = PDec
  module RPDec = RPDec
  module PInfo = Pack_info.Make(Hash)(Inflate)(HDec)(PDec)
  module PEnc = Pack.Stream(Hash)(Deflate)
  module IDec = Index_pack.Lazy(Hash)
  module IEnc = Index_pack.Encoder(Hash)

  (* XXX(dinosaure): I need to explain what is the purpose of this
     module. As the Loose internal module, this module implements the
     logic of pack files. It's little bit more complex than the loose
     file.

     In fact, we have some ways to get an object from a pack file:

     - The first way, the more simple way is to load the IDX file of
       the PACK file, make a PACK decoder, inform the PACK decoder
       than external source is a function which allocates the
       requested object.

       In this way, the decoder wants 2 fixed-size buffers (one to
       inflate and the zlib's window). However, the decoder will
       traverse the path to construct the requested object to
       calculate the biggest needed object. Then, it allocates 2
       buffers of this calculated size and finally reconstruct your
       object - but, it need to save the diff for each application
       which could explose your memory.

       So, this way is the fastest way about initialization (nothing
       to initialize) but the memory allocation is not predictable and
       could be a problem for a large pack file.

     - The second way is to get some informations before to make the
       decoder. To get these informations, we need to read entirely
       one time (one pass) the pack file. Then, we can know the
       biggest object of this pack file and how many buffers we need
       to undelta-ify any objects of this pack file.

       Again, the decoder wants 2 fixed-size buffer (one to inflate
       and the zlib's window) but with the previous computation, we
       can allocate exactly what is needed in the worst case (worst
       case when we want to undelta-ify an object) and reconstruct any
       git object of this pack file.

       However, if the pack file is a /thin/-pack, we will allocate
       again what is needed to get the external object.

       So, at this stage, the memory is semi-predictable. However, we
       can consider this way as the best way because Git is not
       allowed to store in your file-system any /thin/-pack. So, a
       pack decoder should never want an external object.

       From this first pass, we can generate a partial tree of the
       pack file which is equivalent of the idx decoder (but
       partially). From benchmark, obviously, the radix tree is more
       fast than the idx decoder. However, it not contains all objects
       (only non-delta-ified objects), so you should use both.

     - The third way come from the second way with a second pass of
       the pack file to determine if the pack file is a /thin/-pack or
       not. With this information, we can know which objects the pack
       file need and allocate, again, exactly what is needed to get
       any object of this specific pack file.

       Finally, in same time, we can generate a complexe radix tree of
       the pack file and use it instead the decoder.

     So, after this explanation, I decided to implement the second way
     and the third way when we collect all informations needed as long
     as the client use the git repository - I mean, we pass from the
     partial to the total information only when the user asks enough
     to get all object of the pack file.

     The first way is only used to know if the requested object exists
     in the pack file or not - but when the client want to get the Git
     object, we start the first pass of the pack file and make a
     decoder.

     Then, from this information, we update a safe-thread value which
     contains buffers for all loaded pack files. That means, for any
     requested object located on any pack file, we can use safely
     these buffers (instead to allocate all time).

     So, in the git repository, this is the only value which can grow
     automatically (but it's depends on your pack files - so we can
     predict how much memory you need when we look your pack
     files). So, we continue to control the memory consumption. *)

  type loaded =
    { decoder : RPDec.t
    ; idx     : IDec.t
    ; fdp     : FS.Mapper.fd
    ; fdi     : FS.Mapper.fd
    ; info    : PInfo.partial PInfo.t }

  type pack =
    | Exists of { idx : IDec.t
                ; fdi : FS.Mapper.fd }
    | Loaded of loaded
    | Total of  { decoder : RPDec.t
                ; fdp     : FS.Mapper.fd
                ; info    : PInfo.full PInfo.t }

  module Graph = Map.Make(Hash)

  type buffers =
    { objects : Cstruct.t * Cstruct.t * int
    ; hunks   : Cstruct.t
    ; depth   : int }

  type fs_error = FS.error Error.FS.t

  type t =
    { fs      : FS.t
    ; packs   : pack Graph.t Lwt_mvar.t
    ; buffers : buffers Lwt_mvar.t }

  type error =
    [ `Pack_decoder of RPDec.error
    | `Pack_encoder of PEnc.error
    | `Pack_info of PInfo.error
    | `Idx_decoder of IDec.error
    | `Idx_encoder of IEnc.error
    | fs_error
    | `Invalid_hash of Hash.t
    | `Integrity of string
    | `Not_found ]

  let pp_error ppf = function
    | `Pack_decoder err       -> RPDec.pp_error ppf err
    | `Pack_encoder err       -> PEnc.pp_error ppf err
    | `Pack_info err          -> PInfo.pp_error ppf err
    | `Idx_decoder err        -> IDec.pp_error ppf err
    | `Idx_encoder err        -> IEnc.pp_error ppf err
    | #fs_error as err        -> Error.FS.pp_error FS.pp_error ppf err
    | #Error.not_found as err -> Error.pp_not_found ppf err
    | `Invalid_hash hash ->
      Fmt.pf ppf "Unable to load %a, it does not exists in the store"
        Hash.pp hash
    | `Integrity err ->
      Fmt.string ppf err

  (* XXX(dinosaure): to make a _unloaded_ pack object. *)
  let load_index fs path =
    let close fd sys_err =
      FS.Mapper.close fd >>= function
      | Ok ()          -> Lwt.return sys_err
      | Error sys_err' ->
        Log.err (fun l ->
            l "Error while closing the index fd: %a: %a."
              Fpath.pp path FS.Mapper.pp_error sys_err');
        Lwt.return sys_err
    in
    let open Lwt_result in
    (* FIXME: this code is horrible *)
    ((FS.Mapper.openfile fs path
      >!= Error.FS.err_open path
      >>= fun fd -> FS.Mapper.length fd
      >>!= close fd
           >!= Error.FS.err_length path
      >|= fun length -> (fd, length))
     >>= fun (fd, length) -> (
       FS.Mapper.map fd (Int64.to_int length)
       >>!= close fd
       >!= Error.FS.err_map path
       >|= fun map -> (fd, map)
     ) >>= fun (fd, map) -> (
       Lwt.return (IDec.make map)
       >>!= close fd
       >>!= (fun sys_err -> Lwt.return (`Idx_decoder sys_err))
     ) >>= fun decoder_idx -> (
       let hash_idx =
         let basename = Fpath.basename (Fpath.rem_ext path) in
         Scanf.sscanf basename "pack-%s" (fun x -> Hash.of_hex x)
         (* XXX(dinosaure): check if [sscanf] raises an exception. *)
       in
       Log.debug (fun l -> l "IDX file %a is loaded." Hash.pp hash_idx);
       Lwt.return (Ok (hash_idx, decoder_idx, fd)))
    ) >>!= (fun err -> Lwt.return (path, err))

  let v fs lst =
    Lwt_list.map_p (load_index fs) lst >>= fun lst ->
    Lwt_list.fold_left_s (fun acc -> function
        | Ok (hash, idx, fdi) ->
          Lwt.return (Graph.add hash (Exists { idx; fdi; }) acc)
        | Error (path, err) ->
          Log.err (fun l ->
              l "Error while computing the IDX file %a: %a."
                Fpath.pp path pp_error err);
          Lwt.return acc
      ) Graph.empty lst
    >|= fun graph ->
    let packs = Lwt_mvar.create graph in
    let buffers =
      let empty = Cstruct.create 0 in
      Lwt_mvar.create { objects = empty, empty, 0
                      ; hunks = empty
                      ; depth = 1 }
    in
    { fs; packs; buffers; }

  (* XXX(dinosaure): this function update the internal buffers of [t]
     from a /fresh/ information of a pack file. It's thread-safe. *)
  let merge t info =
    Lwt_mvar.take t.buffers >>= fun buffers ->
    let (raw0, raw1, len) = buffers.objects in
    let (raw0', raw1', len') =
      if len < info.PInfo.max_length_object
      then
        let plus0, plus1 =
          Cstruct.create (info.PInfo.max_length_object - len),
          Cstruct.create (info.PInfo.max_length_object - len) in
        (Cstruct.concat [ raw0; plus0 ],
         Cstruct.concat [ raw1; plus1 ],
         info.PInfo.max_length_object)
      else
        (raw0, raw1, len)
    in
    let hunks_length = Cstruct.len buffers.hunks / buffers.depth in
    let depth = info.PInfo.max_depth + 1 in
    let hunks', depth' =
      match buffers.depth < depth,
            hunks_length < info.PInfo.max_length_insert_hunks
      with
      | false, true ->
        Log.debug (fun l -> l ~header:"merge" "The hunks buffer needs to grow from %d to %d."
                      hunks_length info.PInfo.max_length_insert_hunks);
        let plus = Cstruct.create (buffers.depth * (info.PInfo.max_length_insert_hunks - hunks_length)) in
        Cstruct.concat [ buffers.hunks; plus ], buffers.depth
      | true, false ->
        Log.debug (fun l -> l ~header:"merge" "The depth needs to be updated from %d to %d."
                      buffers.depth depth);
        let plus = Cstruct.create ((depth - buffers.depth) * hunks_length) in
        Cstruct.concat [ buffers.hunks; plus ], depth
      | true, true ->
        Log.debug (fun l -> l ~header:"merge" "The depth and the hunks buffer need to be updated, \
                                               %d to %d for the depth and %d to %d for the hunks buffer."
                      buffers.depth depth
                      hunks_length info.PInfo.max_length_insert_hunks);
        let plus =
          Cstruct.create
            ((buffers.depth * (info.PInfo.max_length_insert_hunks - hunks_length))
             + ((depth - buffers.depth) * info.PInfo.max_length_insert_hunks))
        in
        Cstruct.concat [ buffers.hunks; plus ], depth
      | false, false ->
        buffers.hunks, buffers.depth
    in

    Lwt_mvar.put t.buffers { objects = (raw0', raw1', len')
                           ; hunks = hunks'
                           ; depth = depth' }

  module EIDX = struct
    module E = struct
      type state  = IEnc.t
      type result = unit
      type error  = IEnc.error
      type end' = [ `End of state ]
      type rest = [ `Flush of state | `Error of state * error ]
      let flush = IEnc.flush
      let used = IEnc.used_out
      let eval raw state = match IEnc.eval raw state with
        | #end' as v   -> let `End state = v in Lwt.return (`End (state, ()))
        | #rest as res -> Lwt.return res
    end
    include Helper.Encoder(E)(FS)
  end

  let save_idx_file ~fs ~root sequence hash_pack =
    let file = Fmt.strf "pack-%s.idx" (Hash.to_hex hash_pack) in
    let encoder_idx = IEnc.default sequence hash_pack in
    let raw = Cstruct.create 0x8000 in
    let path = Fpath.(root / "objects" / "pack" / file) in
    EIDX.to_file fs path raw encoder_idx >|= function
    | Ok ()                  -> Ok ()
    | Error (`Encoder err)   -> Error (`Idx_encoder err)
    | Error #fs_error as err -> err

  exception Leave of Hash.t

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

  module EPACK = struct
    module E = struct

      type state = {
        get : Graph.key -> Cstruct.t option Lwt.t;
        pack: PEnc.t;
        src : Cstruct.t option;
      }

      type result = {
        tree: (Crc32.t * int64) PEnc.Map.t;
        hash: Hash.t
      }

      type error = PEnc.error
      let empty = Cstruct.create 0
      let option_get ~default = function Some a -> a | None -> default

      let rec eval dst state =
        match
          PEnc.eval (option_get ~default:empty state.src) dst state.pack
        with
        | `End (pack, hash) ->
          Lwt.return (`End ({ state with pack; },
                            { tree = (PEnc.idx pack)
                            ; hash }))
        | `Error (pack, err) -> Lwt.return (`Error ({ state with pack; }, err))
        | `Flush pack        -> Lwt.return (`Flush { state with pack; })
        | `Await pack ->
          match state.src with
          | Some _ ->
            eval dst { state with pack = PEnc.finish pack ; src = None }
          | None   ->
            let hash = PEnc.expect pack in
            state.get hash >>= function
            | None -> Lwt.fail (Leave hash)
            | Some raw ->
              let state = {
                state with pack = PEnc.refill 0 (Cstruct.len raw) pack;
                           src  = Some raw;
              } in
              eval dst state

      let flush off len ({ pack; _ } as state) =
        { state with pack = PEnc.flush off len pack }

      let used { pack; _ } = PEnc.used_out pack

    end
    include Helper.Encoder(E)(FS)
  end

  let save_pack_file ~fs fmt entries get =
    let ztmp = Cstruct.create 0x8000 in
    let filename_pack = fmt (random_string 10) in
    let state = PEnc.default ztmp entries in
    FS.Dir.temp fs >>= fun temp ->
    let path = Fpath.(temp / filename_pack) in
    let raw = Cstruct.create 0x8000 in
    let state = { EPACK.E.get; src = None; pack = state } in
    Lwt.catch (fun () ->
        EPACK.to_file fs ~atomic:false path raw state >|= function
        | Ok { EPACK.E.tree; hash; } ->
          Ok (Fpath.(temp / filename_pack)
             , (fun f -> PEnc.Map.iter (fun k v -> f (k, v)) tree)
             , hash)
        | Error #fs_error as err -> err
        | Error (`Encoder e) -> Error (`Pack_encoder e)
      ) (function
        | Leave hash -> Lwt.return (Error (`Invalid_hash hash))
        | exn        -> Lwt.fail exn (* XXX(dinosaure): should never happen. *)
      )

  let add_exists ~root t hash =
    let filename_idx = Fmt.strf "pack-%s.idx" (Hash.to_hex hash) in
    let path_idx = Fpath.(root / "objects" / "pack" / filename_idx) in
    load_index t.fs path_idx >>= function
    | Error (_, err)          -> Lwt.return (Error err)
    | Ok (hash_idx, idx, fdi) ->
      let pack = Exists { idx; fdi; } in
      Lwt_mvar.take t.packs
      >>= fun packs -> Lwt_mvar.put t.packs (Graph.add hash_idx pack packs)
      >|= fun () -> Ok ()

  let add_total ~root t path info =
    let `Full { PInfo.Full.hash = hash_pack; PInfo.Full.thin; } =
      info.PInfo.state
    in
    if thin then invalid_arg "Impossible to store a thin pack.";
    let filename_pack = Fmt.strf "pack-%s.pack" (Hash.to_hex hash_pack) in
    let filename_idx = Fmt.strf "pack-%s.idx" (Hash.to_hex hash_pack) in
    FS.File.move t.fs path Fpath.(root / "objects" / "pack" / filename_pack)
    >>= function
    | Error err ->
      Lwt.return Error.(v @@ FS.err_move path Fpath.(root / "objects" / "pack" / filename_pack) err)
    | Ok ()     ->
      let path_idx = Fpath.(root / "objects" / "pack" / filename_idx) in
      let sequence = (fun f -> PInfo.Map.iter (fun k v -> f (k, v)) info.PInfo.tree) in
      let encoder_idx = IEnc.default sequence hash_pack in
      (* XXX(dinosaure): we save an IDX file for a future computation
         of git/ocaml-git of this PACK file even if we don't use it -
         we use the complete radix tree. *)
      let raw = Cstruct.create 0x8000 in
      EIDX.to_file t.fs path_idx raw encoder_idx >>= function
      | Error #fs_error as err -> Lwt.return err
      | Error (`Encoder e) -> Lwt.return (Error (`Idx_encoder e))
      | Ok ()              ->
        let path_pack = Fpath.(root / "objects" / "pack" / filename_pack) in
        (FS.Mapper.openfile t.fs path_pack
         >>!= fun sys_err -> Lwt.return (Error.FS.err_open path_pack sys_err))
        >>?= fun fdp ->
        let fun_cache _ = None in
        let fun_idx hash = PInfo.Map.find_opt hash info.PInfo.tree in
        let fun_revidx hash =
          try let (_, ret) = PInfo.Graph.find hash info.PInfo.graph in ret
          with Not_found -> None
        in
        let fun_last _ = Lwt.return None in
        (* XXX(dinosaure): the pack file is not thin. So it does
           not request any external ressource. *)
        (RPDec.make fdp
           fun_cache
           fun_idx
           fun_revidx
           fun_last
         >>!= fun err -> Lwt.return (Error.FS.err_open path_pack err))
        >>?= fun decoder ->
        let pack = Total { decoder; fdp; info; } in
        Lwt_mvar.take t.packs
        >>= fun packs -> Lwt_mvar.put t.packs (Graph.add hash_pack pack packs)
        >>= fun () -> merge t info
        >|= fun () ->
        let count = PInfo.Graph.cardinal info.PInfo.graph in
        Ok (hash_pack, count)

  (* XXX(dinosaure): this function could not be exposed. It used in
     a specific context, when a pack decoder requests an external
     object. This case appear only when the pack file is a
     /thin/-pack. So, we need to allocate some buffers to avoid a
     memory-explosion (if the object is delta-ified) and return a
     /fresh/ raw of the requested object. *)
  let strong_weight_read decoder info hash request =
    let (raw0, raw1, len) =
      Cstruct.create info.PInfo.max_length_object,
      Cstruct.create info.PInfo.max_length_object,
      info.PInfo.max_length_object
    in
    let htmp = Cstruct.create ((info.PInfo.max_depth + 1) * info.PInfo.max_length_insert_hunks) in
    let htmp = Array.init (info.PInfo.max_depth + 1)
        (fun i -> Cstruct.sub htmp (i * info.PInfo.max_length_insert_hunks)
            info.PInfo.max_length_insert_hunks)
    in

    let ztmp = Cstruct.create 0x8000 in
    let window = Inflate.window () in
    (RPDec.get_from_hash ~htmp decoder request (raw0, raw1, len) ztmp window >>= function
      | Ok obj -> Lwt.return (Some (obj.RPDec.Object.kind, obj.RPDec.Object.raw))
      | Error err ->
        Log.err (fun l -> l ~header:"read_and_exclude" "Error when we try to get the object %a from the pack %a: %a."
                    Hash.pp request Hash.pp hash
                    RPDec.pp_error err);
        Lwt.return None)

  (* XXX(dinosaure): [read_and_exclude] is called when a pack decoder
     wants to reconstruct an internal object and the source is
     external - by this way, we can consider the pack file as a /thin/
     pack.

     So this is the biggest function which could allocate a lot.
     Obviously, in a real world, this function could not happen (Git
     takes care about pack file and stores only /non-thin/ pack
     files).

     However, from a [ `Partial ] information, we can not ensure than
     the pack file is not a /thin/-pack. So this function exists for
     the pack decoder when we want to retrieve an external object.

     Then, we exclude the pack itself to avoid a infinite recursion
     when the length of the redirection is 1. If it's upper, firstly,
     what?  then, you could have a infinite loop. About this bug, we
     could change the API of the pack decoder and allow to inform a
     /close/ list to the function. *)
  let rec read_and_exclude ~root t exclude request =
    Lwt_mvar.take t.packs >>= fun packs ->
    Lwt_mvar.put t.packs packs >>= fun () ->
    Lwt_list.fold_left_s (function
        | Some _ as x -> fun _ -> Lwt.return x
        | None ->
          fun (hash, pack) -> match pack with
            | Exists { idx; fdi; } when IDec.mem idx request ->
              (load_partial ~root t hash idx fdi >>= function
                | Ok loaded -> strong_weight_read loaded.decoder loaded.info request hash
                | Error _ -> Lwt.return None)
            | Loaded { idx; decoder; info; _ } when IDec.mem idx request ->
              strong_weight_read decoder info request hash
            | Total { decoder; info; _ } when PInfo.Map.mem request info.PInfo.tree ->
              strong_weight_read decoder info request hash
            | _ -> Lwt.return None)
      None
      (Graph.fold (fun key value acc ->
           if Hash.equal key exclude
           then acc
           else (key, value) :: acc)
          packs [])
    >>= function
    | Some _ as v -> Lwt.return v
    | None ->
      (* XXX(dinosaure): ok, the last chance to retrieve the
         requested object. We lookup in the loose object. *)
      assert false

  (* XXX(dinosaure): this function loads _partially_ a pack file.
     That means we calculate some useful informations from a _not
     loaded_ pack representation (which has only the index decoder)
     and make a new /fresh/ pack decoder.

     It consists to read one time entirely the pack file, calculate
     some informations and store it to use it after - like how much
     we need to decode a git object from this specific pack file.

     This computation can not know how much we need if any
     delta-ified objects of this specific pack file need an external
     git object - at this point, we can consider this pack file as a
     /thin/-pack.

     If we catch any error for any computation, we delete this pack
     file as an available pack file - we lost it forever! *)
  and load_partial ~root t hash decoder_idx fdi =
    let filename_pack = Fmt.strf "pack-%s.pack" (Hash.to_hex hash) in
    let path = Fpath.(root / "objects" / "pack" / filename_pack) in
    let ztmp = Cstruct.create 0x8000 in
    let window = Inflate.window () in
    let close_all fdi fdp sys_err =
      (* XXX(dinosaure): delete from the git repository. *)
      Lwt_mvar.take t.packs >>= fun packs ->
      Lwt_mvar.put t.packs (Graph.remove hash packs) >>= fun () ->
      (FS.Mapper.close fdi
       >>!= fun sys_err' ->
       Log.err (fun l -> l ~header:"log" "Impossible to close the index fd: %a"
                   FS.Mapper.pp_error sys_err');
       Lwt.return ())
      >>= fun _ ->
      (FS.Mapper.close fdp
       >>!= fun sys_err' ->
       Log.err (fun l -> l ~header:"log" "Impossible to close the pack fd: %a"
                   FS.Mapper.pp_error sys_err');
       Lwt.return ())
      >>= fun _ -> Lwt.return sys_err
    in
    (FS.Mapper.openfile t.fs path
     >>!= (fun sys_err ->
         (* XXX(dinosaure): delete from the git repository. *)
         Lwt_mvar.take t.packs >>= fun packs ->
         Lwt_mvar.put t.packs (Graph.remove hash packs) >>= fun () ->
         FS.Mapper.close fdi >|= function
         | Ok () -> sys_err
         | Error sys_err' ->
           Log.err (fun l ->
               l "Got an error while trying to close the index file-descriptor, ignoring: %a."
                 FS.Mapper.pp_error sys_err');
           sys_err)
          >!= Error.FS.err_open path)
    >>?= fun fdp ->
    (let fun_cache  = fun _ -> None in
     let fun_idx    = fun hash -> IDec.find decoder_idx hash in
     let fun_revidx = fun _ -> None in
     let fun_last   = fun hash' -> read_and_exclude ~root t hash hash' in

     RPDec.make fdp fun_cache fun_idx fun_revidx fun_last
     >!= Error.FS.err_length path (* XXX(dinosaure): we know [RPDec.make] try to compute length of the PACK file. *)
     >>!= close_all fdi fdp)
    >>?= fun decoder_pack ->
    (FS.File.open_r t.fs path
     >!= Error.FS.err_open path
     >>!= close_all fdi fdp)
    >>?= fun fd ->
    (let raw = Cstruct.create 0x8000 in
     let stream () =
       FS.File.read raw fd >|= function
       | Ok 0          -> None
       | Ok len        -> Some (Cstruct.sub raw 0 len)
       | Error sys_err ->
         Log.err (fun l ->
             l "Retrieve an error when we read the PACK file %s: %a."
               filename_pack FS.pp_error sys_err);
         None
     in
     let info = PInfo.v hash in
     PInfo.from_stream ~ztmp ~window info stream
     >>!= (fun err -> Lwt.return (`Pack_info err))
     >>!= close_all fdi fdp
     >>!= (fun sys_err ->
         FS.File.close fd >|= function
         | Ok ()          -> sys_err
         | Error sys_err' ->
           Log.err (fun l ->
               l "Impossible to close the fd of the PACK file (to analyze it): \
                  %a." FS.pp_error sys_err');
           sys_err))
    >>?= fun info ->
    let pack =
      { decoder = decoder_pack
      ; fdp
      ; fdi
      ; info
      ; idx = decoder_idx }
    in

    Lwt.return (Ok pack)

  let force_total t loaded =
    let `Partial { PInfo.Partial.hash = hash_pack; delta } =
      loaded.info.PInfo.state
    in
    let hash_of_object obj =
      let ctx = Hash.Digest.init () in
      let hdr = Fmt.strf "%s %Ld\000%!"
          (match obj.RPDec.Object.kind with
           | `Commit -> "commit"
           | `Tree -> "tree"
           | `Tag -> "tag"
           | `Blob -> "blob")
          obj.RPDec.Object.length
      in
      Hash.Digest.feed ctx (Cstruct.of_string hdr);
      Hash.Digest.feed ctx obj.RPDec.Object.raw;
      Hash.Digest.get ctx
    in
    let crc32 obj = match obj.RPDec.Object.from with
      | RPDec.Object.Offset { crc; _ } -> crc
      | RPDec.Object.External _ -> raise (Invalid_argument "Try to get the CRC-32 from an external ressource")
      | RPDec.Object.Direct { crc; _ } -> crc
    in
    let ztmp = Cstruct.create 0x8000 in
    let window = Inflate.window () in
    Lwt_mvar.take t.buffers >>= fun buffers ->
    Lwt_list.map_s
      (fun (offset, hunks_descr) ->
         let length = Cstruct.len buffers.hunks / buffers.depth in
         RPDec.get_from_offset ~htmp:
           (Array.init buffers.depth
              (fun i -> Cstruct.sub buffers.hunks (i * length) length))
           loaded.decoder offset buffers.objects ztmp window >>= function
         | Ok obj ->
           let hash = hash_of_object obj in
           let crc32 = crc32 obj in
           Lwt.return (Ok (hunks_descr, hash, (crc32, offset)))
         | Error err ->
           Log.err (fun l ->
               l "Retrieve an error when we try to reconstruct \
                  the object at the offset %Lx from the PACK file %a: %a."
                 offset Hash.pp hash_pack RPDec.pp_error err);
           Lwt.return (Error ()))
      delta
    >>= fun lst -> Lwt_mvar.put t.buffers buffers >>= fun () -> Lwt.return lst
    >>= Lwt_list.fold_left_s (fun ((tree, graph) as acc) -> function
        | Ok (hunks_descr, hash, ((_, offset) as value)) ->
          let tree = PInfo.Map.add hash value tree in
          let graph =
            let open PInfo in
            let depth_source, _ = match hunks_descr.HDec.reference with
              | HDec.Offset rel_off ->
                (try Graph.find Int64.(sub offset rel_off) graph
                 with Not_found -> 0, None)
              | HDec.Hash hash_source ->
                try match Map.find_opt hash_source tree with
                  | Some (_, abs_off) -> Graph.find abs_off graph
                  | None -> 0, None
                with Not_found -> 0, None
            in
            Graph.add offset (depth_source + 1, Some hash) graph
          in
          Lwt.return (tree, graph)
        | Error _ -> Lwt.return acc
      ) (loaded.info.PInfo.tree, loaded.info.PInfo.graph)
    >>= fun (tree', graph') ->
    let is_total =
      PInfo.Graph.for_all
        (fun _ -> function (_, Some _) -> true | (_, None) -> false) graph'
    in
    if is_total then
      (* XXX(samoht): for_all_p is uselesse here *)
      Lwt_list.for_all_p (fun (_, hunks_descr) ->
          let open PInfo in
          match hunks_descr.HDec.reference with
          | HDec.Offset _  -> Lwt.return true
          | HDec.Hash hash -> Lwt.return (Map.mem hash tree'))
        delta
      >|= fun is_not_thin ->
      Ok (loaded.decoder,
          loaded.fdp,
          { loaded.info with PInfo.tree = tree'
                           ; graph = graph'
                           ; state = `Full { PInfo.Full.hash = hash_pack
                                           ; thin = not is_not_thin } })
    else
      Fmt.kstrf (fun x -> Lwt.return (Error (`Integrity x)))
        "Impossible to get all informations from the pack file: %a."
        Hash.pp hash_pack

  exception Found of (Hash.t * (Crc32.t * int64))

  let lookup t hash =
    Lwt_mvar.take t.packs >>= fun packs ->
    Lwt_mvar.put t.packs packs >|= fun () ->
    try Graph.iter (fun hash_pack -> function
        | Loaded { idx; _ }
        | Exists { idx; _ } ->
          (match IDec.find idx hash with
           | Some (crc32, offset) -> raise (Found (hash_pack, (crc32, offset)))
           | None -> ())
        | Total { info; _ } ->
          (match PInfo.Map.find_opt hash info.PInfo.tree with
           | Some (crc32, offset) -> raise (Found (hash_pack, (crc32, offset)))
           | None -> ()))
        packs;
      None
    with Found (hash_pack, offset) ->
      Some (hash_pack, offset)

  let mem t hash =
    lookup t hash >|= function
    | Some _ -> true
    | None   -> false

  let list t =
    Lwt_mvar.take t.packs >>= fun packs ->
    Lwt_mvar.put t.packs packs >|= fun () ->
    Graph.fold (fun _ -> function
        | Loaded { idx; _ } ->
          IDec.fold idx (fun hash _ acc -> hash :: acc)
        | Exists { idx; _ } ->
          IDec.fold idx (fun hash _ acc -> hash :: acc)
        | Total { info; _ } ->
          fun acc ->
            PInfo.Map.fold (fun hash _ acc ->
                hash :: acc
              ) info.PInfo.tree acc
      ) packs []

  let read ~root ~ztmp ~window t hash =
    lookup t hash >>= function
    | None -> Lwt.return Error.(v err_not_found)
    | Some (pack_hash, _) ->
      Lwt_mvar.take t.packs >>= fun packs ->
      Lwt_mvar.put t.packs packs >>= fun () ->
      match Graph.find pack_hash packs with
      | Exists { idx; fdi; }  ->
        (load_partial ~root t pack_hash idx fdi >>= function
          | Ok loaded ->
            merge t loaded.info >>= fun () ->
            Lwt_mvar.take t.packs >>= fun packs ->
            Lwt_mvar.put t.packs (Graph.add pack_hash (Loaded loaded) packs) >>= fun () ->
            Lwt_mvar.take t.buffers >>= fun buffers ->
            let length = Cstruct.len buffers.hunks / buffers.depth in
            RPDec.get_from_hash
              ~htmp:(Array.init buffers.depth (fun i -> Cstruct.sub buffers.hunks (i * length) length))
              loaded.decoder hash buffers.objects ztmp window
            >>!= (fun err -> Lwt.return (`Pack_decoder err)) >>= fun ret ->
            Lwt_mvar.put t.buffers buffers >|= fun () ->
            ret
          | Error _ as err -> Lwt.return err)
      | Loaded loaded ->
        Lwt_mvar.take t.buffers >>= fun buffers ->
        let length = Cstruct.len buffers.hunks / buffers.depth in
        RPDec.get_from_hash
          ~htmp:(Array.init buffers.depth (fun i ->
              Cstruct.sub buffers.hunks (i * length) length))
          loaded.decoder hash buffers.objects ztmp window
        >>!= (fun err -> Lwt.return (`Pack_decoder err))
        >>= fun ret -> Lwt_mvar.put t.buffers buffers
        >|= fun () -> ret
      | Total { decoder; _ } ->
        Lwt_mvar.take t.buffers >>= fun buffers ->
        let length = Cstruct.len buffers.hunks / buffers.depth in
        RPDec.get_from_hash
          ~htmp:(Array.init buffers.depth (fun i ->
              Cstruct.sub buffers.hunks (i * length) length))
          decoder hash buffers.objects ztmp window
        >>!= (fun err -> Lwt.return (`Pack_decoder err)) >>= fun ret ->
        Lwt_mvar.put t.buffers buffers >|= fun () ->
        ret
      | exception Not_found ->
        (* XXX(dinosaure): could appear in only one case: if the
           version of the [t.packs] in [lookup_p] has the pack but
           something (like an error) appeared with this pack and
           deleted it in the current version of [t.packs] took by this
           function.

           So, if this case appears, that means the pack file expected
           is malformed (and inner objects are losted with it). *)
        Lwt.return (Error `Not_found)

  let size ~root ~ztmp ~window t hash =
    lookup t hash >>= function
    | None -> Lwt.return (Error `Not_found)
    | Some (pack_hash, _) ->
      Lwt_mvar.take t.packs >>= fun packs ->
      Lwt_mvar.put t.packs packs >>= fun () ->
      match Graph.find pack_hash packs with
      | Exists { idx; fdi; }  ->
        (load_partial ~root t pack_hash idx fdi >>= function
          | Ok loaded ->
            merge t loaded.info >>= fun () ->
            Lwt_mvar.take t.packs >>= fun packs ->
            Lwt_mvar.put t.packs (Graph.add pack_hash (Loaded loaded) packs) >>= fun () ->
            Lwt_mvar.take t.buffers >>= fun buffers ->
            RPDec.length loaded.decoder hash ztmp window
            >>!= (fun err -> Lwt.return (`Pack_decoder err)) >>= fun ret ->
            Lwt_mvar.put t.buffers buffers >>= fun () ->
            Lwt.return ret
          | Error _ as err -> Lwt.return err)
      | Loaded loaded ->
        Lwt_mvar.take t.buffers >>= fun buffers ->
        RPDec.length loaded.decoder hash ztmp window
        >>!= (fun err -> Lwt.return (`Pack_decoder err)) >>= fun ret ->
        Lwt_mvar.put t.buffers buffers >>= fun () ->
        Lwt.return ret
      | Total { decoder; _ } ->
        Lwt_mvar.take t.buffers >>= fun buffers ->
        RPDec.length decoder hash ztmp window
        >>!= (fun err -> Lwt.return (`Pack_decoder err)) >>= fun ret ->
        Lwt_mvar.put t.buffers buffers >>= fun () ->
        Lwt.return ret
      | exception Not_found ->
        Lwt.return (Error `Not_found)
end
