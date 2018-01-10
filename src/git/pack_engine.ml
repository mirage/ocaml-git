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

let src = Logs.Src.create "git.pack" ~doc:"Git pack engine"
module Log = (val Logs.src_log src : Logs.LOG)

module type S = sig

  module Hash: S.HASH
  module Inflate: S.INFLATE
  module Deflate: S.DEFLATE
  module FS: S.FS

  module PACKDecoder: Unpack.DECODER
    with module Hash = Hash
     and type Mapper.error = FS.error
     and type Mapper.fd = FS.Mapper.fd
     and module Inflate = Inflate

  module PACKEncoder: Pack.ENCODER
    with module Hash = Hash
     and module Deflate = Deflate

  module IDXDecoder: Index_pack.LAZY
    with module Hash = Hash

  module IDXEncoder: Index_pack.ENCODER
    with module Hash = Hash

  module Pack_info: Pack_info.S
    with module Hash = Hash
     and module Inflate = Inflate

  type t
  type loaded

  type error =
    [ `PackDecoder of PACKDecoder.error
    | `PackEncoder of PACKEncoder.error
    | `PackInfo of Pack_info.error
    | `IdxDecoder of IDXDecoder.error
    | `IdxEncoder of IDXEncoder.error
    | `FS of FS.error
    | `Invalid_hash of Hash.t
    | `IO of string
    | `Integrity of string
    | `Not_found ]

  val pp_error: error Fmt.t

  val v: Fpath.t list -> t Lwt.t

  val add_total:
       root:Fpath.t
    -> t
    -> Fpath.t
    -> Pack_info.full Pack_info.t
    -> (Hash.t * int, error) result Lwt.t

  val add_exists:
       root:Fpath.t
    -> t
    -> Hash.t
    -> (unit, error) result Lwt.t

  val merge: t -> [> Pack_info.partial | Pack_info.full ] Pack_info.t -> unit Lwt.t

  val load_index:
       Fpath.t
    -> (Hash.t * IDXDecoder.t * FS.Mapper.fd, Fpath.t * error) result Lwt.t

  val load_partial:
       root:Fpath.t
    -> t
    -> Hash.t
    -> IDXDecoder.t
    -> FS.Mapper.fd
    -> (loaded, error) result Lwt.t

  val force_total:
       t
    -> loaded
    -> ((PACKDecoder.t * FS.Mapper.fd * Pack_info.full Pack_info.t), error) result Lwt.t

  val lookup: t -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t

  val mem: t -> Hash.t -> bool Lwt.t

  val list: t -> Hash.t list Lwt.t

  val read:
       root:Fpath.t
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> t
    -> Hash.t
    -> (PACKDecoder.Object.t, error) result Lwt.t

  val size:
       root:Fpath.t
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> t
    -> Hash.t
    -> (int, error) result Lwt.t

  val save_idx_file:
       root:Fpath.t
    -> (Hash.t * (Crc32.t * int64)) Radix.sequence
    -> Hash.t
    -> (unit, error) result Lwt.t

  val save_pack_file:
       (string -> string)
    -> (PACKEncoder.Entry.t * PACKEncoder.Delta.t) list
    -> (Hash.t -> Cstruct.t option Lwt.t)
    -> (Fpath.t * (Hash.t * (Crc32.t * int64)) Radix.sequence * Hash.t, error) result Lwt.t
end

module Make (H: S.HASH) (FS: S.FS) (I: S.INFLATE) (D: S.DEFLATE):
  S with module Hash = H
     and module Inflate = I
     and module Deflate = D
     and module FS = FS
= struct

  module Hash = H
  module Inflate = I
  module Deflate = D
  module FS = FS

  module Pack_info = Pack_info.Make(H)(I)
  module PACKDecoder = Unpack.MakeDecoder(H)(struct
      type error = FS.error
      include FS.Mapper
    end)(I)
  module PACKEncoder = Pack.MakePACKEncoder(H)(D)
  module IDXDecoder = Index_pack.Lazy(H)
  module IDXEncoder = Index_pack.Encoder(H)

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
    { decoder : PACKDecoder.t
    ; idx     : IDXDecoder.t
    ; fdp     : FS.Mapper.fd
    ; fdi     : FS.Mapper.fd
    ; info    : Pack_info.partial Pack_info.t }

  type pack =
    | Exists of { idx : IDXDecoder.t
                ; fdi : FS.Mapper.fd }
    | Loaded of loaded
    | Total of  { decoder : PACKDecoder.t
                ; fdp     : FS.Mapper.fd
                ; info    : Pack_info.full Pack_info.t }

  module Graph = Map.Make(Hash)

  type t =
    { packs   : pack Graph.t Lwt_mvar.t
    ; buffers : buffers Lwt_mvar.t }
  and buffers =
    { objects : Cstruct.t * Cstruct.t * int
    ; hunks   : Cstruct.t
    ; depth   : int }

  type error =
    [ `PackDecoder of PACKDecoder.error
    | `PackEncoder of PACKEncoder.error
    | `PackInfo of Pack_info.error
    | `IdxDecoder of IDXDecoder.error
    | `IdxEncoder of IDXEncoder.error
    | `FS of FS.error
    | `Invalid_hash of Hash.t
    | `IO of string
    | `Integrity of string
    | `Not_found ]

  let pp_error ppf = function
    | `PackDecoder err   -> Fmt.pf ppf "(`PackDecoder %a)" PACKDecoder.pp_error err
    | `PackEncoder err   -> Fmt.pf ppf "(`PackEncoder %a)" PACKEncoder.pp_error err
    | `IdxDecoder err    -> Fmt.pf ppf "(`IdxDecoder %a)" IDXDecoder.pp_error err
    | `IdxEncoder err    -> Fmt.pf ppf "(`IdxEncoder %a)" IDXEncoder.pp_error err
    | `FS sys_err        -> Fmt.pf ppf "(`FS %a)" FS.pp_error sys_err
    | `Invalid_hash hash -> Fmt.pf ppf "(`Invalid_hash %a)" Hash.pp hash
    | `IO err            -> Fmt.pf ppf "(`IO %s)" err
    | `Integrity err     -> Fmt.pf ppf "(`Integrity %s)" err
    | `Not_found         -> Fmt.pf ppf "`Not_found"
    | `PackInfo err      -> Fmt.pf ppf "(`Pack_info %a)" Pack_info.pp_error err

  (* XXX(dinosaure): to make a _unloaded_ pack object. *)
  let load_index path =
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
    ((FS.Mapper.openfile path >>!=
      (fun sys_err -> Lwt.return (`FS sys_err))
      >>= fun fd -> FS.Mapper.length fd
      >>!= (fun sys_err -> close fd sys_err)
      >>!= (fun sys_err -> Lwt.return (`FS sys_err))
      >|= fun length -> (fd, length))
     >>= fun (fd, length) -> (
       FS.Mapper.map ~share:false fd (Int64.to_int length)
       >>!= (fun sys_err -> close fd sys_err)
       >>!= (fun sys_err -> Lwt.return (`FS sys_err))
       >|= fun map -> (fd, map)
     ) >>= fun (fd, map) -> (
       Lwt.return (IDXDecoder.make map)
       >>!= close fd
       >>!= (fun sys_err -> Lwt.return (`IdxDecoder sys_err))
     ) >>= fun decoder_idx -> (
       let hash_idx =
         let basename = Fpath.basename (Fpath.rem_ext path) in
         Scanf.sscanf basename "pack-%s" (fun x -> Hash.of_hex x)
         (* XXX(dinosaure): check if [sscanf] raises an exception. *)
       in
       Log.debug (fun l -> l "IDX file %a is loaded." Hash.pp hash_idx);
       Lwt.return (Ok (hash_idx, decoder_idx, fd)))
     ) >>!= (fun err -> Lwt.return (path, err))

  let v lst =
    Lwt_list.map_p load_index lst >>= fun lst ->
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
    { packs; buffers; }

  (* XXX(dinosaure): this function update the internal buffers of [t]
     from a /fresh/ information of a pack file. It's thread-safe. *)
  let merge t info =
    Lwt_mvar.take t.buffers >>= fun buffers ->
    let (raw0, raw1, len) = buffers.objects in
    let (raw0', raw1', len') =
      if len < info.Pack_info.max_length_object
      then
        let plus0, plus1 =
          Cstruct.create (info.Pack_info.max_length_object - len),
          Cstruct.create (info.Pack_info.max_length_object - len) in
        (Cstruct.concat [ raw0; plus0 ],
         Cstruct.concat [ raw1; plus1 ],
         info.Pack_info.max_length_object)
      else
        (raw0, raw1, len)
    in
    let hunks_length = Cstruct.len buffers.hunks / buffers.depth in
    let depth = info.Pack_info.max_depth + 1 in
    let hunks', depth' =
      match buffers.depth < depth,
            hunks_length < info.Pack_info.max_length_insert_hunks
      with
      | false, true ->
        Log.debug (fun l -> l ~header:"merge" "The hunks buffer needs to grow from %d to %d."
                      hunks_length info.Pack_info.max_length_insert_hunks);
        let plus = Cstruct.create (buffers.depth * (info.Pack_info.max_length_insert_hunks - hunks_length)) in
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
                      hunks_length info.Pack_info.max_length_insert_hunks);
        let plus =
          Cstruct.create
            ((buffers.depth * (info.Pack_info.max_length_insert_hunks - hunks_length))
             + ((depth - buffers.depth) * info.Pack_info.max_length_insert_hunks))
        in
        Cstruct.concat [ buffers.hunks; plus ], depth
      | false, false ->
        buffers.hunks, buffers.depth
    in

    Lwt_mvar.put t.buffers { objects = (raw0', raw1', len')
                           ; hunks = hunks'
                           ; depth = depth' }

  let save_idx_file ~root sequence hash_pack =
    let filename_idx = Fmt.strf "pack-%s.idx" (Hash.to_hex hash_pack) in
    let encoder_idx = IDXEncoder.default sequence hash_pack in
    let module E = struct
      type state  = IDXEncoder.t
      type raw    = Cstruct.t
      type result = unit
      type error  = IDXEncoder.error
      let raw_length = Cstruct.len
      let raw_blit   = Cstruct.blit
      type end' = [ `End of state ]
      type rest = [ `Flush of state | `Error of state * error ]
      let eval raw state = match IDXEncoder.eval raw state with
        | #end' as v   -> let `End state = v in Lwt.return (`End (state, ()))
        | #rest as res -> Lwt.return res
      let flush = IDXEncoder.flush
      let used = IDXEncoder.used_out
    end in
    let raw = Cstruct.create 0x8000 in
    FS.File.open_w ~mode:0o644 Fpath.(root / "objects" / "pack" / filename_idx) >>= function
    | Error sys_err -> Lwt.return (Error (`FS sys_err))
    | Ok fd ->
      Helper.safe_encoder_to_file
        ~limit:50 (module E) FS.File.write fd raw encoder_idx
      >>= function
      | Error err ->
        ((FS.File.close fd >>= function
           | Error sys_err ->
             Log.err (fun l -> l ~header:"v_total" "Impossible to close the file %a: %a."
                         Fpath.pp Fpath.(root / "objects" / "pack" / filename_idx)
                         FS.pp_error sys_err);
             Lwt.return ()
           | Ok () -> Lwt.return ()) >>= fun () -> match err with
         | `Stack ->
           Lwt.return (Error (`IO (Fmt.strf "Impossible to store the IDX file: %a."
                                           Fpath.pp Fpath.(root / "objects" / "pack" / filename_idx))))
         | `Writer sys_err ->
           Lwt.return (Error (`FS sys_err))
         | `Encoder err ->
           Lwt.return (Error (`IdxEncoder err)))
      | Ok () ->
        FS.File.close fd
        >>!= (fun sys_err -> Lwt.return (`FS sys_err))

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

  let save_pack_file fmt entries get =
    let ztmp = Cstruct.create 0x8000 in
    let state = PACKEncoder.default ztmp entries in
    let option_get ~default = function Some a -> a | None -> default in
    let module E = struct
      type state =
        { pack : PACKEncoder.t
        ; src  : Cstruct.t option }
      type raw = Cstruct.t
      type result =
        { tree : (Crc32.t * int64) PACKEncoder.Radix.t
        ; hash : Hash.t }
      type error = PACKEncoder.error
      let raw_length = Cstruct.len
      let raw_blit   = Cstruct.blit
      let empty = Cstruct.create 0
      let rec eval dst state =
        match PACKEncoder.eval (option_get ~default:empty state.src) dst state.pack with
        | `End (pack, hash) ->
          Lwt.return (`End ({ state with pack; },
                            { tree = (PACKEncoder.idx pack)
                            ; hash }))
        | `Error (pack, err) -> Lwt.return (`Error ({ state with pack; }, err))
        | `Flush pack        -> Lwt.return (`Flush { state with pack; })
        | `Await pack ->
          match state.src with
          | Some _ -> eval dst { pack = PACKEncoder.finish pack ; src = None }
          | None   ->
            let hash = PACKEncoder.expect pack in
            get hash >>= function
            | Some raw ->
              eval dst { pack = PACKEncoder.refill 0 (Cstruct.len raw) pack
                       ; src = Some raw }
            | None -> Lwt.fail (Leave hash)
      let flush off len ({ pack; _ } as state) =
        { state with pack = PACKEncoder.flush off len pack }
      let used { pack; _ } = PACKEncoder.used_out pack
    end in
    let filename_pack = fmt (random_string 10) in
    let ( >!= ) = Lwt_result.bind_lwt_err in
    FS.Dir.temp () >>= fun temp ->
    FS.File.open_w ~mode:0o644 Fpath.(temp / filename_pack) >>= function
    | Error sys_err -> Lwt.return (Error (`FS sys_err))
    | Ok fd ->
      let raw = Cstruct.create 0x8000 in
      (let close value =
         FS.File.close fd >>= function
         | Error sys_err ->
           Log.err (fun l -> l ~header:"save_pack_file" "Impossible to close the pack file %a: %a."
                       Fpath.pp Fpath.(temp / filename_pack)
                       FS.pp_error sys_err);
           Lwt.return value
         | Ok () -> Lwt.return value
       in
       let open Lwt_result in
       (Lwt.try_bind (fun () ->
            Helper.safe_encoder_to_file ~limit:50 (module E)
              FS.File.write fd raw { E.src = None; pack = state })
           (function
             | Ok _ as v -> Lwt.return v
             | (Error `Stack
               | Error (`Writer _)
               | Error (`Encoder _)) as err -> Lwt.return err)
           (function
             | Leave hash -> Lwt.return (Error (`Invalid_hash hash))
             | exn        -> Lwt.fail exn)) (* XXX(dinosaure): should never happen. *)
       >!= close
       >>= (fun ret -> close (Ok ret)))
      >|= function
      | Error (`Invalid_hash _ ) as err -> err
      | Error `Stack ->
        Fmt.kstrf (fun x -> Error (`IO x))
          "Impossible to store the PACK file: %a." Fpath.pp
          Fpath.(temp / filename_pack)
      | Error (`Writer sys_err) -> Error (`FS sys_err)
      | Error (`Encoder err)    -> Error (`PackEncoder err)
      | Ok { E.tree; E.hash; }  ->
        Ok (Fpath.(temp / filename_pack)
           , PACKEncoder.Radix.to_sequence tree, hash)

  let add_exists ~root t hash =
    let filename_idx = Fmt.strf "pack-%s.idx" (Hash.to_hex hash) in
    let path_idx = Fpath.(root / "objects" / "pack" / filename_idx) in
    load_index path_idx >>= function
    | Error (_, err)          -> Lwt.return (Error err)
    | Ok (hash_idx, idx, fdi) ->
      let pack = Exists { idx; fdi; } in
      Lwt_mvar.take t.packs
      >>= fun packs -> Lwt_mvar.put t.packs (Graph.add hash_idx pack packs)
      >|= fun () -> Ok ()

  let add_total ~root t path info =
    let `Full { Pack_info.Full.hash = hash_pack; Pack_info.Full.thin; } =
      info.Pack_info.state
    in
    if thin
    then raise (Invalid_argument "Impossible to store a thin pack in your git repository.");
    let filename_pack = Fmt.strf "pack-%s.pack" (Hash.to_hex hash_pack) in
    let filename_idx = Fmt.strf "pack-%s.idx" (Hash.to_hex hash_pack) in
    FS.File.move path Fpath.(root / "objects" / "pack" / filename_pack) >>= function
    | Error err -> Lwt.return (Error (`FS err))
    | Ok () ->
      FS.File.open_w ~mode:0o644 Fpath.(root / "objects" / "pack" / filename_idx) >>= function
      | Error err -> Lwt.return (Error (`FS err))
      | Ok fdi ->
        let sequence = Pack_info.Radix.to_sequence info.Pack_info.tree in
        let encoder_idx = IDXEncoder.default sequence hash_pack in
        (* XXX(dinosaure): we save an IDX file for a future
           computation of git/ocaml-git of this PACK file even if we
           don't use it - we use the complete radix tree. *)
        let module E = struct
          type state  = IDXEncoder.t
          type raw    = Cstruct.t
          type result = unit
          type error  = IDXEncoder.error
          let raw_length = Cstruct.len
          let raw_blit   = Cstruct.blit
          type end' = [ `End of state ]
          type rest = [ `Flush of state | `Error of state * error ]
          let eval raw state = match IDXEncoder.eval raw state with
            | #end' as v   -> let `End state = v in Lwt.return (`End (state, ()))
            | #rest as res -> Lwt.return res
          let flush = IDXEncoder.flush
          let used = IDXEncoder.used_out
        end in
        let raw = Cstruct.create 0x8000 in
        Helper.safe_encoder_to_file
          ~limit:50 (module E) FS.File.write fdi raw encoder_idx
        >>= function
        | Error err ->
          ((FS.File.close fdi >>= function
             | Error sys_err ->
               Log.err (fun l ->
                   l "Got an error while trying to close the file %a: %a."
                     Fpath.pp Fpath.(root / "objects" / "pack" / filename_idx)
                     FS.pp_error sys_err);
               Lwt.return ()
             | Ok () -> Lwt.return ()) >>= fun () -> match err with
           | `Stack ->
             Lwt.return (Error (`IO (Fmt.strf "Impossible to store the IDX file: %a."
                                             Fpath.pp Fpath.(root / "objects" / "pack" / filename_idx))))
           | `Writer sys_err ->
             Lwt.return (Error (`FS sys_err))
           | `Encoder err ->
             Lwt.return (Error (`IdxEncoder err)))
        | Ok () -> FS.File.close fdi >>= function
          | Error err -> Lwt.return (Error (`FS err))
          | Ok () ->
            (FS.Mapper.openfile Fpath.(root / "objects" / "pack" / filename_pack)
             >>!= fun sys_err -> Lwt.return (`FS sys_err))
            >>?= fun fdp ->
            let fun_cache _ = None in
            let fun_idx hash = Pack_info.Radix.lookup info.Pack_info.tree hash in
            let fun_revidx hash =
              try let (_, ret) = Pack_info.Graph.find hash info.Pack_info.graph in ret
              with Not_found -> None
            in
            let fun_last _ = Lwt.return None in
            (* XXX(dinosaure): the pack file is not thin. So it does
               not request any external ressource. *)
            (PACKDecoder.make fdp
               fun_cache
               fun_idx
               fun_revidx
               fun_last
             >>!= fun err -> Lwt.return (`FS err))
            >>?= fun decoder ->
            let pack = Total { decoder; fdp; info; } in
            Lwt_mvar.take t.packs
            >>= fun packs -> Lwt_mvar.put t.packs (Graph.add hash_pack pack packs)
            >>= fun () -> merge t info
            >>= fun () ->
            let count = Pack_info.Graph.cardinal info.Pack_info.graph in
            Lwt.return (Ok (hash_pack, count))

  (* XXX(dinosaure): this function could not be exposed. It used in
     a specific context, when a pack decoder requests an external
     object. This case appear only when the pack file is a
     /thin/-pack. So, we need to allocate some buffers to avoid a
     memory-explosion (if the object is delta-ified) and return a
     /fresh/ raw of the requested object. *)
  let strong_weight_read decoder info hash request =
    let (raw0, raw1, len) =
      Cstruct.create info.Pack_info.max_length_object,
      Cstruct.create info.Pack_info.max_length_object,
      info.Pack_info.max_length_object
    in
    let htmp = Cstruct.create ((info.Pack_info.max_depth + 1) * info.Pack_info.max_length_insert_hunks) in
    let htmp = Array.init (info.Pack_info.max_depth + 1)
        (fun i -> Cstruct.sub htmp (i * info.Pack_info.max_length_insert_hunks)
            info.Pack_info.max_length_insert_hunks)
    in

    let ztmp = Cstruct.create 0x8000 in
    let window = Inflate.window () in
    (PACKDecoder.optimized_get ~h_tmp:htmp decoder request (raw0, raw1, len) ztmp window >>= function
      | Ok obj -> Lwt.return (Some (obj.PACKDecoder.Object.kind, obj.PACKDecoder.Object.raw))
      | Error err ->
        Log.err (fun l -> l ~header:"read_and_exclude" "Error when we try to get the object %a from the pack %a: %a."
                    Hash.pp request Hash.pp hash
                    PACKDecoder.pp_error err);
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
            | Exists { idx; fdi; } when IDXDecoder.mem idx request ->
              (load_partial ~root t hash idx fdi >>= function
                | Ok loaded -> strong_weight_read loaded.decoder loaded.info request hash
                | Error _ -> Lwt.return None)
            | Loaded { idx; decoder; info; _ } when IDXDecoder.mem idx request ->
              strong_weight_read decoder info request hash
            | Total { decoder; info; _ } when Pack_info.Radix.mem info.Pack_info.tree request ->
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
    (FS.Mapper.openfile Fpath.(root / "objects" / "pack" / filename_pack)
     >>!= (fun sys_err ->
         (* XXX(dinosaure): delete from the git repository. *)
         Lwt_mvar.take t.packs >>= fun packs ->
         Lwt_mvar.put t.packs (Graph.remove hash packs) >>= fun () ->
         FS.Mapper.close fdi >|= function
         | Ok () -> sys_err
         | Error sys_err' ->
           Log.err (fun l ->
               l "Got an error while trying to close the index fd: %a."
                 FS.Mapper.pp_error sys_err');
           sys_err)
     >>!= (fun sys_err -> Lwt.return (`FS sys_err)))
    >>?= fun fdp ->
    (let fun_cache  = fun _ -> None in
     let fun_idx    = fun hash -> IDXDecoder.find decoder_idx hash in
     let fun_revidx = fun _ -> None in
     let fun_last   = fun hash' -> read_and_exclude ~root t hash hash' in

     (PACKDecoder.make fdp fun_cache fun_idx fun_revidx fun_last
      >>!= (fun err -> Lwt.return (`FS err)))
     >>!= (fun sys_err -> close_all fdi fdp sys_err))
    >>?= fun decoder_pack ->
    ((FS.File.open_r ~mode:0o644 Fpath.(root / "objects" / "pack" / filename_pack)
      >>!= (fun sys_err -> Lwt.return (`FS sys_err)))
     >>!= (fun sys_err -> close_all fdi fdp sys_err))
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
     let info = Pack_info.v hash in
     (Pack_info.from_stream ~ztmp ~window info stream
      >>!= (fun err -> Lwt.return (`PackInfo err)))
     >>!= (fun sys_err -> close_all fdi fdp sys_err)
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
    let `Partial { Pack_info.Partial.hash = hash_pack; delta } =
      loaded.info.Pack_info.state
    in
    let hash_of_object obj =
      let ctx = Hash.Digest.init () in
      let hdr = Fmt.strf "%s %Ld\000%!"
          (match obj.PACKDecoder.Object.kind with
           | `Commit -> "commit"
           | `Tree -> "tree"
           | `Tag -> "tag"
           | `Blob -> "blob")
          obj.PACKDecoder.Object.length
      in
      Hash.Digest.feed ctx (Cstruct.of_string hdr);
      Hash.Digest.feed ctx obj.PACKDecoder.Object.raw;
      Hash.Digest.get ctx
    in
    let crc32 obj = match obj.PACKDecoder.Object.from with
      | PACKDecoder.Object.Offset { crc; _ } -> crc
      | PACKDecoder.Object.External _ -> raise (Invalid_argument "Try to get the CRC-32 from an external ressource")
      | PACKDecoder.Object.Direct { crc; _ } -> crc
    in
    let ztmp = Cstruct.create 0x8000 in
    let window = Inflate.window () in
    Lwt_mvar.take t.buffers >>= fun buffers ->
    Lwt_list.map_s
      (fun (offset, hunks_descr) ->
         let length = Cstruct.len buffers.hunks / buffers.depth in
         PACKDecoder.optimized_get' ~h_tmp:
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
                 offset Hash.pp hash_pack PACKDecoder.pp_error err);
           Lwt.return (Error ()))
      delta
    >>= fun lst -> Lwt_mvar.put t.buffers buffers >>= fun () -> Lwt.return lst
    >>= Lwt_list.fold_left_s (fun ((tree, graph) as acc) -> function
        | Ok (hunks_descr, hash, ((_, offset) as value)) ->
          let tree = Pack_info.Radix.bind tree hash value in
          let graph =
            let open Pack_info in
            let depth_source, _ = match hunks_descr.PACKDecoder.H.reference with
              | PACKDecoder.H.Offset rel_off ->
                (try Graph.find Int64.(sub offset rel_off) graph
                 with Not_found -> 0, None)
              | PACKDecoder.H.Hash hash_source ->
                try match Radix.lookup tree hash_source with
                  | Some (_, abs_off) -> Graph.find abs_off graph
                  | None -> 0, None
                with Not_found -> 0, None
            in
            Graph.add offset (depth_source + 1, Some hash) graph
          in
          Lwt.return (tree, graph)
        | Error _ -> Lwt.return acc
      ) (loaded.info.Pack_info.tree, loaded.info.Pack_info.graph)
    >>= fun (tree', graph') ->
    let is_total =
      Pack_info.Graph.for_all
        (fun _ -> function (_, Some _) -> true | (_, None) -> false) graph'
    in
    if is_total then
      (* XXX(samoht): for_all_p is uselesse here *)
      Lwt_list.for_all_p (fun (_, hunks_descr) ->
          let open Pack_info in
          match hunks_descr.PACKDecoder.H.reference with
          | PACKDecoder.H.Offset _  -> Lwt.return true
          | PACKDecoder.H.Hash hash -> Lwt.return (Radix.mem tree' hash))
        delta
      >|= fun is_not_thin ->
      Ok (loaded.decoder,
          loaded.fdp,
          { loaded.info with Pack_info.tree = tree'
                           ; graph = graph'
                           ; state = `Full { Pack_info.Full.hash = hash_pack
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
          (match IDXDecoder.find idx hash with
           | Some (crc32, offset) -> raise (Found (hash_pack, (crc32, offset)))
           | None -> ())
        | Total { info; _ } ->
          (match Pack_info.Radix.lookup info.Pack_info.tree hash with
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
          IDXDecoder.fold idx (fun hash _ acc -> hash :: acc)
        | Exists { idx; _ } ->
          IDXDecoder.fold idx (fun hash _ acc -> hash :: acc)
        | Total { info; _ } ->
          fun acc ->
            Pack_info.Radix.fold (fun (hash, _) acc ->
                hash :: acc
              ) acc info.Pack_info.tree
      ) packs []

  let read ~root ~ztmp ~window t hash =
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
            let length = Cstruct.len buffers.hunks / buffers.depth in
            PACKDecoder.optimized_get
              ~h_tmp:(Array.init buffers.depth (fun i -> Cstruct.sub buffers.hunks (i * length) length))
              loaded.decoder hash buffers.objects ztmp window
            >>!= (fun err -> Lwt.return (`PackDecoder err)) >>= fun ret ->
            Lwt_mvar.put t.buffers buffers >|= fun () ->
            ret
          | Error _ as err -> Lwt.return err)
      | Loaded loaded ->
        Lwt_mvar.take t.buffers >>= fun buffers ->
        let length = Cstruct.len buffers.hunks / buffers.depth in
        PACKDecoder.optimized_get
          ~h_tmp:(Array.init buffers.depth (fun i ->
              Cstruct.sub buffers.hunks (i * length) length))
          loaded.decoder hash buffers.objects ztmp window
        >>!= (fun err -> Lwt.return (`PackDecoder err))
        >>= fun ret -> Lwt_mvar.put t.buffers buffers
        >|= fun () -> ret
      | Total { decoder; _ } ->
        Lwt_mvar.take t.buffers >>= fun buffers ->
        let length = Cstruct.len buffers.hunks / buffers.depth in
        PACKDecoder.optimized_get
          ~h_tmp:(Array.init buffers.depth (fun i ->
              Cstruct.sub buffers.hunks (i * length) length))
          decoder hash buffers.objects ztmp window
        >>!= (fun err -> Lwt.return (`PackDecoder err)) >>= fun ret ->
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
            PACKDecoder.length loaded.decoder hash ztmp window
            >>!= (fun err -> Lwt.return (`PackDecoder err)) >>= fun ret ->
            Lwt_mvar.put t.buffers buffers >>= fun () ->
            Lwt.return ret
          | Error _ as err -> Lwt.return err)
      | Loaded loaded ->
        Lwt_mvar.take t.buffers >>= fun buffers ->
        PACKDecoder.length loaded.decoder hash ztmp window
        >>!= (fun err -> Lwt.return (`PackDecoder err)) >>= fun ret ->
        Lwt_mvar.put t.buffers buffers >>= fun () ->
        Lwt.return ret
      | Total { decoder; _ } ->
        Lwt_mvar.take t.buffers >>= fun buffers ->
        PACKDecoder.length decoder hash ztmp window
        >>!= (fun err -> Lwt.return (`PackDecoder err)) >>= fun ret ->
        Lwt_mvar.put t.buffers buffers >>= fun () ->
        Lwt.return ret
      | exception Not_found ->
        Lwt.return (Error `Not_found)
end
