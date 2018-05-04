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

module Option =
struct
  let ( >>= ) v f = match v with Some v -> f v | None -> None
  let ( >|= ) v f = match v with Some v -> Some (f v) | None -> None
end

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

  type ('mmu, 'location) r =
    { mmu              : 'mmu
    ; with_cstruct     : 'mmu -> pack -> int -> (('location * Cstruct.t) -> unit Lwt.t) -> unit Lwt.t
    ; with_cstruct_opt : 'mmu -> int option -> (('location * Cstruct.t) option -> unit Lwt.t) -> unit Lwt.t
    ; free             : 'mmu -> 'location -> unit Lwt.t }
  and pack = Pack of Hash.t | Unrecorded

  type error =
    [ `Pack_decoder of RPDec.error
    | `Pack_encoder of PEnc.error
    | `Pack_info of PInfo.error
    | `Idx_decoder of IDec.error
    | `Idx_encoder of IEnc.error
    | FS.error Error.FS.t
    | Inflate.error Error.Inf.t
    | Error.Decoder.t
    | `Invalid_hash of Hash.t
    | `Delta of PEnc.Delta.error
    | `Not_found ]

  val pp_error: error Fmt.t

  val v: FS.t -> Fpath.t list -> t Lwt.t
  val lookup: t -> Hash.t -> (Hash.t * (Crc32.t * int64)) option
  val list: t -> Hash.t list
  val mem: t -> Hash.t -> bool

  val add:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> ('mmu, 'location) r
    -> t
    -> Fpath.t
    -> [ `Normalized of PInfo.path ] PInfo.t
    -> (unit, error) result Lwt.t

  val read:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> to_result:(RPDec.Object.t -> ('value, error) result Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> ('mmu, 'location) r
    -> t
    -> Hash.t
    -> ('value, error) result Lwt.t

  val size:
       root:Fpath.t
    -> read_loose:(Hash.t -> (RPDec.kind * Cstruct.t) option Lwt.t)
    -> ztmp:Cstruct.t
    -> window:Inflate.window
    -> FS.t
    -> t
    -> Hash.t
    -> (int, error) result Lwt.t
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

  type fs_error = FS.error Error.FS.t
  type inf_error = Inflate.error Error.Inf.t

  type error =
    [ `Pack_decoder of RPDec.error
    | `Pack_encoder of PEnc.error
    | `Pack_info of PInfo.error
    | `Idx_decoder of IDec.error
    | `Idx_encoder of IEnc.error
    | inf_error
    | fs_error
    | Error.Decoder.t (* XXX(dinosaure): trick about [to_result] function. *)
    | `Invalid_hash of Hash.t
    | `Delta of PEnc.Delta.error
    | `Not_found ]

  type ('mmu, 'location) r =
    { mmu              : 'mmu
    ; with_cstruct     : 'mmu -> pack -> int -> (('location * Cstruct.t) -> unit Lwt.t) -> unit Lwt.t
    ; with_cstruct_opt : 'mmu -> int option -> (('location * Cstruct.t) option -> unit Lwt.t) -> unit Lwt.t
    ; free             : 'mmu -> 'location -> unit Lwt.t }
  and pack = Pack of Hash.t | Unrecorded

  let empty_cstruct = Cstruct.create 0

  module Exists =
  struct

    (* XXX(dinosaure): Voici l'état [exists]. C'est le premier /entry-point/
       pour un fichier PACK déjà existant. La seule opération, sans promotion,
       disponible dans cet état est [lookup]. On y charge uniquement le fichier
       IDX associé - et ainsi savoir tout les objets qui compose le fichier
       PACK. Ainsi, si l'utilisateur demande un objet depuis ce fichier PACK, il
       devrait y avoir une promotion vers l'état [loaded].

       Il n'y a que très peu d'allocation dans cet état, juste un [mmap] total
       du fichier IDX. *)

    type t =
      { index     : IDec.t
      ; hash_pack : Hash.t
      ; fd        : FS.Mapper.fd }

    let lookup { index; _ } hash = IDec.find index hash
    let mem { index; _ } hash = IDec.mem index hash
    let fold f { index; _ } a = IDec.fold index f a

    let make fs path =
      let err_idx_decoder err = `Idx_decoder err in

      let ( <.> ) f g = fun x -> f (g x) in
      let ( >>?= ) = Lwt_result.bind in
      let ( >>|= ) = Lwt_result.bind_result in
      (* ('a, 'e) result Lwt.t -> ('e -> (unit, 'e) result Lwt.t) -> ('a, 'e) result Lwt.t *)
      let ( >>!= ) v f = v >>= function Ok _ as v -> Lwt.return v | Error err -> f err in

      FS.Mapper.openfile fs path                        >|= Rresult.R.reword_error (Error.FS.err_open path)
      >>?= fun fd -> FS.Mapper.length fd                >|= Rresult.R.reword_error (Error.FS.err_length path)
      >>?= fun ln -> FS.Mapper.map fd (Int64.to_int ln) >|= Rresult.R.reword_error (Error.FS.err_map path)
      >>|= (Rresult.R.reword_error err_idx_decoder <.> IDec.make)
      >>?= fun id ->
       let hash_pack =
         let basename = Fpath.basename (Fpath.rem_ext path) in
         Scanf.sscanf basename "pack-%s" (fun x -> Hash.of_hex x) in
       Lwt.return_ok { index = id; hash_pack; fd; }
      >>!= fun er -> FS.Mapper.close fd                 >|= Rresult.R.reword_error (Error.FS.err_close path)
      >>?= fun () -> Lwt.return_error er
  end

  module Loaded =
  struct

    (* XXX(dinosaure): Voici l'état [loaded], il ne peut dériver que de l'état
       [exists]. Cet état est le pire sous le plan de l'allocation et de la
       recherche des objets. On y est associé au fichier IDX et au fichier PACK.
       L'opération [lookup] est disponible et est mémoizé avec une ~Hashtbl~
       interne (disponible dans ~info.PInfo.index).

       Seule l'opération [read] peut compléter cet ~Hashtbl~ - puisqu'on a
       besoin de savoir la position absolue de l'objet, son /checksum/ et sa
       taille qui ne sont disponible qu'en ayant lu entièrement l'objet (le
       fichier IDX ne nous renseigne pas sur la taille par exemple).

       Ensuite, l'opération [read] complète petit à petit les opérations. Il
       peut y avoir une promotion vers l'état [Normalized] seulement quand
       ~info.PInfo.paths~ contient tout les objets référencés par le fichier IDX
       (à vrai dire, on peut même passé à l'état [resolved] puisque si
       ~info.PInfo.paths~ est complet, ~info.PInfo.index~ l'est aussi). *)

    type t =
      { index : IDec.t
      ; pack  : RPDec.t
      ; info  : [ `Pass ] PInfo.t
      ; fdi   : FS.Mapper.fd
      ; fdp   : FS.Mapper.fd
      ; mutable thin : bool }

    let lookup { index; info; _ } hash =
      match Hashtbl.find_opt info.PInfo.index hash with
      | Some (crc, abs_off, _) -> Some (crc, abs_off)
      | None -> IDec.find index hash

    let mem { index; info; _ } hash =
      Hashtbl.mem info.PInfo.index hash || IDec.mem index hash

    let fold f { index; _ } a = IDec.fold index f a

    (* XXX(dinosaure): avec l'extraction d'un objet, on connait son /path/ de
       delta-ification. Il s'agit de le transformer en /apth/ selon les termes
       de ~PInfo~ ensuite. *)
    let rec object_to_delta ?(depth = 1) = function
      | RPDec.Object.External { hash; length; } -> PInfo.Unresolved { hash; length; }
      | RPDec.Object.Internal { offset; length; _ } -> PInfo.Internal { abs_off = offset; length; }
      | RPDec.Object.Delta { descr; base; inserts; _ } -> PInfo.Delta { hunks_descr = descr; inserts; depth; from = object_to_delta ~depth:(depth + 1) base; }

    (* XXX(dinosaure): depuis un /path/, on fait un tableau pouvant contenir
       TOUT les /insert hunks/ pour reconstruire le dit objet. Cependant,
       [delta] n'est pas forcément résolu ! Cela veut dire que le /path/ donné
       est peut être incomplet et on va donc pouvoir stocker dans ce tableau
       seulement quelques niveaux nécessaires à la delta-ification.

       L'algorithme derrière l'extraction d'un objet peut s'en sortir même si le
       /path/ n'est pas complet - il va juste se mettre à allouer des [string]
       en lieu et place d'utiliser ce [Cstruct.t]. *)
    let split_of_delta cs delta =
      let depth =
        let rec go acc = function
          | PInfo.Unresolved _ | PInfo.Internal _ -> acc
          | PInfo.Delta { from; _ } -> go (acc + 1) from in
        go 1 delta in
      let arr = Array.make depth empty_cstruct in
      let rec fill idx off = function
        | PInfo.Unresolved _ | PInfo.Internal _ -> ()
        | PInfo.Delta { inserts; from; _ } ->
          Array.unsafe_set arr idx (Cstruct.sub cs off inserts);
          fill (idx + 1) (off + inserts) from in
      fill 0 0 delta; arr

    (* XXX(dinosaure): calculer la taille nécessaire pour stocker tout les
       /insert hunks/ selon le /path/ [delta]. *)
    let length_of_delta delta =
      let rec go acc = function
        | PInfo.Unresolved _ -> acc
        | PInfo.Internal _ -> acc
        | PInfo.Delta { inserts; from; _ } -> go (acc + inserts) from in
      go 0 delta

    let size { pack; _ } ~ztmp ~window hash =
      RPDec.length pack hash ztmp window >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)

    (* XXX(dinosaure): voici la fonction [read] disponible à l'état [loaded].

       ## Arguments

       [root]: [.git]

       [mmu]: unité de management de la mémoire - c'est l'acronyme le plus
       significatif sur l'objectif de cette variable. C'est l'état
       /mutable/global/ qu'on renseignera à chaque allocation.

       [with_cstruct]: malloc

       [with_optional_malloc]: optional malloc

       [free]: free

       [to_result]: Cette fonction doit avoir un seul objectif, prendre
       l'/ownership/ sur le [Cstruct.t] donné. Cela veut dire que le [Cstruct.t]
       donné est celui que nous a donné [with_cstruct]. Il s'agit de le libérer
       (voir [free]) et cette fonction doit tout simplement reprendre
       l'ownership. Cela peut être un simple [cstruct_copy] mais, dans ce
       contexte, on peut donner le [Value.to_result] qui s'assure de bien
       prendre l'/ownership/ (pour les objets Git et spécifiquement pour l'objet
       Blob qui équivaut en tout point à un [cstruct_copy]) et de retourner la
       représentation OCaml de l'objet Git. Cette fonction peut échouer.

       [ztmp]: zlib buffer

       [window]: zlib window

       [loaded]: l'état [loaded]

       [hash]: le hash de l'objet

       ## Allocation

       Comme on peut le constater, [to_result] semble être la fonction la plus
       critique dans tout ces arguments et c'est le cas. L'idée de ces
       abstraction et de laisser le client choisir la politique d'allocation.
       Ces allocations sont nécessaires pour extraire l'objet et l'on peut
       imaginer un cache LRU derrière pour quelques [Cstruct.t].

       Ensuite, on s'assure de libérer ces [Cstruct.t] associés à une /location/
       le plus rapidement possible avec [free]. Bien entendu, cela n'arrive
       seulement qu'après l'application de [to_result]. Si [to_result] est juste
       la fonction identité (ce qui peut être le cas), vous devriez avoir un
       problème sur le contenu qui risque d'être changeant si les [Cstruct.t]
       reçus par [with_cstruct] sont réutilisés.

       [to_result] donc doit __allouer__ la même ou une nouvelle forme de la
       valeur pour en garantir l'/ownership/.

       Enfin, on assure pas que les allocations demandées sont suffisantes pour
       extraire l'objet. C'est parce que les /paths/ ne sont pas résolus que ce
       que l'on peut demander avec [with_optional_cstruct] soit partiel pour
       l'extraction - cela peut alors entraîner l'allocation de [string]
       incontrôlé.

       ## Mémoization

       Bien entendu, pour chaque lecture, on résoud le /path/ de l'objet
       demandé. On mets 2 informations à jour, la première est la ~Hashtbl~
       ~info.PInfo.paths~ et la deuxième est savoir si le fichier PACK est
       /thin/ ou pas.

       Pour la deuxième information, vu que le fichier PACK ne peut venir que de
       l'état [exists], ce n'est normalement pas possible que le fichier PACK
       soit /thin/ - mais on sait jamais. *)
    let read
        (type mmu) (type location) (type value)
        ~root
        ~(to_result:RPDec.Object.t -> (value, error) result Lwt.t)
        ~ztmp ~window
        (r:(mmu, location) r)
        ({ pack; info; index; _ } as t) hash
      : [ `Error of error | `Promote of value | `Return of value ] Lwt.t =
      lookup t hash |> function
      | None -> Lwt.return (`Error (`Pack_decoder (RPDec.Invalid_hash hash)))
      | Some (_, abs_off) ->
        RPDec.needed_from_offset t.pack abs_off ztmp window >>= function
        | Error err -> Lwt.return (`Error (`Pack_decoder err))
        | Ok length ->
          let res = ref None in
          let delta = Hashtbl.find_opt info.PInfo.delta abs_off in

          (r.with_cstruct r.mmu Unrecorded (length * 2) @@ fun (loc_raw, raw) ->
           let raw = Cstruct.sub raw 0 length, Cstruct.sub raw length length, length in

           r.with_cstruct_opt r.mmu Option.(delta >|= length_of_delta) @@ fun hraw ->
           let htmp = Option.(delta >>= fun delta -> hraw >|= fun (_, cs) -> split_of_delta cs delta) in

           let deliver () = r.free r.mmu loc_raw >>= fun () -> match Option.(hraw >|= fst) with
             | Some loc -> r.free r.mmu loc
             | None -> Lwt.return_unit in

           RPDec.get_from_hash ?htmp pack hash raw ztmp window
           >>= fun x -> res := Some (deliver, x); Lwt.return_unit)

          >>= (fun () -> match !res with
              | None -> assert false
              | Some (deliver, res) ->
                Rresult.R.reword_error (fun err -> `Pack_decoder err) res
                |> Lwt.return
                >>?= fun obj -> to_result obj
                >>?= fun res -> Lwt.return_ok (obj, res)
                >>= fun res -> deliver () >|= fun () -> res)
          >|= function
            | Ok (obj, value) ->
              let crc, abs_off, length =
                RPDec.Object.first_crc_exn obj,
                RPDec.Object.first_offset_exn obj,
                RPDec.Object.length obj in
              Hashtbl.add info.PInfo.index hash (crc, abs_off, length);
              Hashtbl.add info.PInfo.delta abs_off (object_to_delta obj.RPDec.Object.from);

              let () = match obj.RPDec.Object.from with
                | RPDec.Object.External _ -> t.thin <- true
                | RPDec.Object.Internal _ -> ()
                | RPDec.Object.Delta { base; _ } ->
                  let rec go = function
                    | RPDec.Object.Delta { base; } -> go base
                    | RPDec.Object.External _ -> t.thin <- true
                    | RPDec.Object.Internal _ -> () in
                  go base in

              if Hashtbl.length info.PInfo.delta = IDec.cardinal index
              then `Promote value
              else `Return value
            | Error err -> `Error err

    let make_pack_decoder ~read_and_exclude ~idx fs path =
      let ( >>!= ) v f = v >>= function Ok _ as v -> Lwt.return v | Error err -> f err in

      FS.Mapper.openfile fs path
      >|= Rresult.R.reword_error (Error.FS.err_open path)
      >>?= fun fd ->
      let fun_cache  = fun _ -> None in
      let fun_idx    = idx in
      let fun_revidx = fun _ -> None in
      let fun_last   = read_and_exclude in

      RPDec.make fd fun_cache fun_idx fun_revidx fun_last
      >|= Rresult.R.reword_error (Error.FS.err_length path)
      >>?= fun pack -> Lwt.return_ok (fd, pack)
      >>!= fun er -> FS.Mapper.close fd >|= Rresult.R.reword_error (Error.FS.err_close path)
      >>?= fun () -> Lwt.return_error er

    (* XXX(dinosaure): voici la fonction de promotion de l'état [exists] à
       l'état [loaded]. La fonction [read_and_exclude] est expliqué plus bas -
       elle dépends de tout les états de chaque fichiers PACK.

       Ici enfin, on utilise le fichier IDX - et pas la ~Hashtbl~ qui n'est pas
       complète - en vrai, on pourrait utiliser la ~Hashtbl~. *)
    let make ~root ~read_and_exclude fs exists =
      let path = Fpath.(root / "objects" / "pack" / Fmt.strf "pack-%s.pack" (Hash.to_hex exists.Exists.hash_pack)) in
      let index = exists.Exists.index in
      let info = PInfo.v exists.Exists.hash_pack in

      let ( >>!= ) v f = v >>= function Ok _ as v -> Lwt.return v | Error err -> f err in

      make_pack_decoder ~read_and_exclude ~idx:(IDec.find exists.Exists.index) fs path
      >>?= fun (fd, pack) ->  Lwt.return_ok { index; pack; info; fdi = exists.Exists.fd; fdp = fd; thin = false }
      >>!= fun er -> FS.Mapper.close exists.Exists.fd >|= Rresult.R.reword_error Error.FS.err_sys_map
      >>?= fun () -> Lwt.return_error er
  end

  module Normalized =
  struct

    (* XXX(dinosaure): Voici l'état [normalized] qui est l'/entry-point/ pour
       les fichiers PACK venant du réseau (clone, pull, fetch). Cet état est
       surtout un état pour le dissocié de [resolved] et enclenché la seconde
       /pass/. La deuxième /pass/ va résoudre les paths des objets delta-ifiés
       and demandant des allocations explicites à l'utilisateur.

       Avec cette résolution, on va résoudre les paths pour ensuite les merger
       entre eux et avoir l'approximation nécessaire à l'extraction de tout les
       objets du dit fichier PACK. Cette résolution va nous permettre aussi de
       savoir si un des objets nécessaire à la delta-ification est externe au
       fichier PACK ou non (ce qui devrait être le cas). *)

    type t =
      { pack  : RPDec.t
      ; path  : Fpath.t
      ; info  : [ `Normalized of PInfo.path ] PInfo.t
      ; fd    : FS.Mapper.fd
      ; mutable thin : bool }

    (* XXX(dinosaure): cette fonction permet de créer un tableau de [Cstruct.t]
       à partir du /path/ - qui peut être partiel. *)
    let split_of_delta cs delta =
      let depth =
        let rec go acc = function
          | PInfo.Unresolved _ | PInfo.Internal _ -> acc
          | PInfo.Delta { from; _ } -> go (acc + 1) from in
        go 0 delta in
      let arr = Array.make depth empty_cstruct in
      let rec fill idx off = function
        | PInfo.Unresolved _ | PInfo.Internal _ -> ()
        | PInfo.Delta { inserts; from; _ } ->
          Array.unsafe_set arr idx (Cstruct.sub cs off inserts);
          fill (idx + 1) (off + inserts) from in
      fill 0 0 delta; arr

    let length_of_delta delta =
      let rec go acc = function
        | PInfo.Unresolved _ -> acc
        | PInfo.Internal _ -> acc
        | PInfo.Delta { inserts; from; _ } -> go (acc + inserts) from in
      go 0 delta

    let length_of_path path =
      let rec go acc = function
        | PInfo.Load _ -> acc
        | PInfo.Patch { hunks; src; _ } -> go (acc + hunks) src in
      go 0 path

    let digest cs =
      let ctx = Hash.Digest.init () in
      Hash.Digest.feed ctx cs;
      Hash.Digest.get ctx

    let make_from_info ~read_and_exclude fs path_tmp info =
      let idx hash = match Hashtbl.find_opt info.PInfo.index hash with
        | Some (crc, abs_off, _) -> Some (crc, abs_off)
        | None -> None in

      Loaded.make_pack_decoder ~read_and_exclude ~idx fs path_tmp >>= function
      | Ok (fd, pack) ->
        Lwt.return_ok { pack; path = path_tmp; info; fd; thin = false }
      | Error _ as err -> Lwt.return err

    exception Fail of RPDec.error

    (* XXX(dinosaure): la seconde /pass/ permet de résoudre les objets
       delta-ifiés et ainsi, par la même occasion, savoir si le fichier PACK est
       /thin/ ou pas. Avec cette résolution, on peut passer de l'état
       [normalized] à l'état [resolved].

       On pourrait déplacer ce code dans le module [Resolved] (TODO).

       Puisque les résolutions sont faites séquentiellement, au niveau de
       l'allocation, un seul buffer qui peut grandir est nécessaire - il n'y
       aura pas de /data-race condition/ dans ce contexte de résolution.info

       NOTE: on pourrait faire une résolution en concurrence mais cela
       compliquerait la gestion des allocations au niveau utilisateur (pour
       justement éviter la /data-race condition/) bien que cette gestion soit
       possible puisque on demande à l'utilisation une fonction [free]
       permettant de notifier l'utilisateur que la ressource demandé associé à
       une [location] (qui peut sémantiquement être une /mutex/) est libéré. *)
    let second_pass
      (type mmu) (type location)
      ~ztmp
      ~window
      (r:(mmu, location) r)
      normalized =
      let resolve (abs_off, delta) =
        RPDec.needed_from_offset normalized.pack abs_off ztmp window >>= function
        | Error err -> Lwt.fail (Fail err)
        | Ok needed ->
          r.with_cstruct r.mmu Unrecorded (needed * 2) @@ fun (loc_raw, raw) ->
          let raw = Cstruct.sub raw 0 needed, Cstruct.sub raw needed needed, needed in

          r.with_cstruct_opt r.mmu (Some (length_of_delta delta)) @@ fun hraw ->
          let htmp = Option.(hraw >|= fun (_, cs) -> split_of_delta cs delta) in

          RPDec.get_from_offset ?htmp normalized.pack abs_off raw ztmp window >>= function
          | Error err -> Lwt.fail (Fail err)
          | Ok obj ->
            let hash = digest obj.RPDec.Object.raw in
            let crc, abs_off =
              RPDec.Object.first_crc_exn obj,
              RPDec.Object.first_offset_exn obj in
            Hashtbl.add normalized.info.PInfo.index hash (crc, abs_off, needed);
            Hashtbl.add normalized.info.PInfo.delta abs_off (Loaded.object_to_delta obj.RPDec.Object.from);

            let () = match obj.RPDec.Object.from with
              | RPDec.Object.External _ -> normalized.thin <- true
              | RPDec.Object.Internal _ -> ()
              | RPDec.Object.Delta { base; _ } ->
                let rec go = function
                  | RPDec.Object.Delta { base; _ } -> go base
                  | RPDec.Object.External _ -> normalized.thin <- true
                  | RPDec.Object.Internal _ -> () in
                go base in

            r.free r.mmu loc_raw
            >>= fun () -> match hraw with
            | Some (loc, _) -> r.free r.mmu loc
            | None -> Lwt.return_unit in

      Lwt.catch
        (fun () ->
           Lwt_list.iter_s
             resolve
             (Hashtbl.fold (fun k v a -> (k, v) :: a) normalized.info.PInfo.delta []
              |> List.sort (fun (ka, _) (kb, _) -> Int64.compare ka kb))
           >>= fun () -> Lwt.return_ok ())
        (function Fail err -> Lwt.return_error (`Pack_decoder err)
                | exn -> Lwt.fail exn)
  end

  let with_buffer buff f =
    let c = ref None in
    buff (fun buf ->
        f buf >|= fun x ->
        c := Some x
      ) >|= fun () ->
    match !c with
    | Some x -> x
    | None   -> assert false

  module Resolved =
  struct

    (* XXX(dinosaure): voici l'état [resolved], on y est presque. Dans cette
       état, on a résolu tout les objets delta-ifiés et, sauf pour les objets
       extérieurs au fichier PACK, on est sur un modèle où l'allocation est
       déterminé - c'est à dire que on vous ne demandera jamais plus que ce
       qu'on vous a déjà demandé. En effet, à ce stade, puisque tout les objets
       sont résolus, on est capable de savoir le plus gros objet du fichier PACK
       et on est capable de savoir strictement les buffers nécessaires pour
       sauvegarder pendant la récursion les /insert hunks/ (et, en cela,
       appliquer successivement les patchs).

       On pourrait même aller jusqu'à faire une fonction /tail-rec/ puisqu'on a
       déterminé les sources nécessaires à la delta-fication. *)

    type t =
      { pack       : RPDec.t
      ; index      : (Hash.t, Crc32.t * int64 * int) Hashtbl.t
      ; delta      : (int64, PInfo.delta) Hashtbl.t
      ; hash_pack  : Hash.t
      ; path_delta : PInfo.path
      ; fd         : FS.Mapper.fd
      ; buff       : (buffer -> unit Lwt.t) -> unit Lwt.t
      ; thin       : bool }
    and buffer =
      { hunks : Cstruct.t array
      ; buffer : Cstruct.t * Cstruct.t * int
      ; depth : int
      ; deliver : unit -> unit Lwt.t }

    let stream_of_path _ = assert false

    let mem { index; _ } hash = Hashtbl.mem index hash

    let fold f { index; _ } a = Hashtbl.fold (fun hash (crc, abs_off, _) a -> f hash (crc, abs_off) a) index a

    let depth_of_path path =
      let rec go acc = function
        | PInfo.Load _ -> acc
        | PInfo.Patch { src; _ } -> go (acc + 1) src in
      go 1 path

    let split_of_path cs path =
      let depth = depth_of_path path in
      let arr = Array.make depth empty_cstruct in
      let rec fill idx off = function
        | PInfo.Load _ -> ()
        | PInfo. Patch { hunks; src; _ } ->
          Array.unsafe_set arr idx (Cstruct.sub cs off hunks);
          fill (idx + 1) (off + hunks) src in
      fill 0 0 path; arr

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

    (* XXX(dinosaure): à ce stade, on peut sauvegarder le fichier IDX si le
       fichier PACK est pas /thin/. C'est une fonction abstraite pour, depuis
       une sequence (version itérable d'une structure), sauvegarde un fichier
       IDX selon le hash du fichier PACK - un fichier IDX est forcément associé
       à un fichier PACK. *)
    let store_idx_file ~root fs sequence hash_pack =
      let file = Fmt.strf "pack-%s.idx" (Hash.to_hex hash_pack) in
      let encoder_idx = IEnc.default sequence hash_pack in
      let raw = Cstruct.create 0x8000 in (* XXX(dinosaure): as argument? *)
      let path = Fpath.(root / "objects" / "pack" / file) in
      EIDX.to_file fs path raw encoder_idx >|= function
      | Ok ()                  -> Ok ()
      | Error (`Encoder err)   -> Error (`Idx_encoder err)
      | Error #fs_error as err -> err

    let make_buffer
        (type mmu) (type location)
        (r:(mmu, location) r)
        hash_pack length_hunks length_buffer path_delta : (buffer -> unit Lwt.t) -> unit Lwt.t =
      let ret = ref None in

      let make () =
        r.with_cstruct r.mmu (Pack hash_pack) (length_hunks + (length_buffer * 2)) @@ fun (loc, buffer) ->

        let hunks = Cstruct.sub buffer 0 length_hunks in
        let hunks = split_of_path hunks path_delta in

        let depth = depth_of_path path_delta in

        let buffer = Cstruct.sub buffer length_hunks (length_buffer * 2) in
        let buffer = Cstruct.sub buffer 0 length_buffer, Cstruct.sub buffer length_buffer length_buffer, length_buffer in

        let deliver () = r.free r.mmu loc in

        ret := Some { hunks; buffer; deliver; depth; };
        Lwt.return_unit in

      (* XXX(dinosaure): bypass value restriction. *)
      let make () = make () >>= fun () -> match !ret with
        | Some ret -> Lwt.return ret
        | None -> assert false in

      let pool = Lwt_pool.create 4 make in
      Lwt_pool.use pool

    (* XXX(dinosaure): Cette fonction permet de passer de l'état [normalized] à
       l'état [resolved]. On applique ainsi la /second_pass/. Ensuite, si le
       fichier PACK est /thin/, on retourne un état qui continue d'utiliser le
       fichier PACK qui devrait se retrouver dans le dossier ~tmp~. Sinon, on
       sauvegarde le fichier IDX dans le dépôt git et cette fonction va
       __déplacer__ le fichier PACK temporaire dans le dépôt git - on
       s'appliquera bien à fermer le /file-descriptor/ du fichier PACK
       temporaire.

       Il s'agira ensuite de passer à l'état [total]. Ce dernier s'appliquera à
       faire la troisième phase ou non. Il faut donc bien saisir que l'état
       [resolved] n'est qu'un état de passage comme l'état [noramlized] - à la
       différence de l'état [loaded] qui peut être utilisé à défaut de devoir
       allouer de la mémoire.

       La politique d'allocation est bien entendu déterminé mais concurrente !
       *)
    let make_from_normalized
        (type mmu) (type location)
        ~root
        ~read_and_exclude
        ~ztmp
        ~window
        fs
        (r:(mmu, location) r)
        normalized =
      Normalized.second_pass ~ztmp ~window r normalized >>= function
      | Error _ as err -> Lwt.return err
      | Ok () ->
        let info = PInfo.resolve ~length:(Hashtbl.length normalized.info.PInfo.delta) normalized.info in
        let path = Fpath.(root / "objects" / "pack" / Fmt.strf "pack-%s.idx" (Hash.to_hex info.PInfo.hash_pack)) in
        let `Resolved path_delta = info.PInfo.state in

        if normalized.Normalized.thin
        then Loaded.make_pack_decoder ~read_and_exclude ~idx:Option.(fun hash -> Hashtbl.find_opt info.PInfo.index hash >|= fun (crc, abs_off, _) -> (crc, abs_off)) fs path
          >>?= fun (fd, pack) ->

          let length_hunks = Normalized.length_of_path path_delta in
          let length_objrw = Hashtbl.fold (fun k (_, _, v) -> max v) info.PInfo.index 0 in
          let buff = make_buffer r length_hunks length_objrw path_delta in

          Lwt.return_ok { pack
                        ; index = info.PInfo.index
                        ; delta = info.PInfo.delta
                        ; hash_pack = info.PInfo.hash_pack
                        ; path_delta
                        ; fd
                        ; buff
                        ; thin = normalized.Normalized.thin }
        else
          let sequence f = Hashtbl.iter (fun k (crc, abs_off, _) -> f (k, (crc, abs_off))) info.PInfo.index in

          store_idx_file ~root fs sequence info.PInfo.hash_pack
          >>?= fun () -> FS.File.move fs normalized.Normalized.path path >|= Rresult.R.reword_error (Error.FS.err_move normalized.Normalized.path path)
          >>?= fun () -> FS.Mapper.close normalized.Normalized.fd >|= Rresult.R.reword_error (Error.FS.err_close normalized.Normalized.path)
          >>?= fun () -> Loaded.make_pack_decoder ~read_and_exclude ~idx:Option.(fun hash -> Hashtbl.find_opt info.PInfo.index hash >|= fun (crc, abs_off, _) -> (crc, abs_off)) fs path
          >>?= fun (fd, pack) ->

          let length_hunks = Normalized.length_of_path path_delta in
          let length_objrw = Hashtbl.fold (fun k (_, _, v) -> max v) info.PInfo.index 0 in
          let buff = make_buffer r length_hunks length_objrw path_delta in

          Lwt.return_ok { pack
                        ; index = info.PInfo.index
                        ; delta = info.PInfo.delta
                        ; hash_pack = info.PInfo.hash_pack
                        ; path_delta
                        ; fd
                        ; buff
                        ; thin = normalized.Normalized.thin }

    let make_from_loaded
        (type mmu) (type location)
        ~read_and_exclude
        ~ztmp
        ~window
        fs
        (r:(mmu, location) r)
        loaded =
      let info = PInfo.normalize ~length:(Hashtbl.length loaded.Loaded.info.PInfo.delta) loaded.Loaded.info in
      let info = PInfo.resolve ~length:(Hashtbl.length loaded.Loaded.info.PInfo.delta) info in
      let `Resolved path_delta = info.PInfo.state in

      let length_hunks = Normalized.length_of_path path_delta in
      let length_objrw = Hashtbl.fold (fun k (_, _, v) -> max v) info.PInfo.index 0 in
      let buff = make_buffer r length_hunks length_objrw path_delta in

      FS.Mapper.close loaded.Loaded.fdi >|= Rresult.R.reword_error Error.FS.err_sys_map >>?= fun () ->
      Lwt.return_ok { pack = loaded.Loaded.pack
                    ; index = info.PInfo.index
                    ; delta = info.PInfo.delta
                    ; hash_pack = info.PInfo.hash_pack
                    ; path_delta
                    ; fd = loaded.Loaded.fdp
                    ; buff
                    ; thin = loaded.Loaded.thin }

    (* XXX(dinosaure): cette fonction permet de passer de l'etat [exists]
       directement à l'état [resolved]. Cette fonction se base sur une
       /assumption/ importante, un fichier PACK dans un dépôt git qui est
       forcément à la base dans l'état [exists] est non-/thin/. Dans ce cas, la
       résolution des objets delta-ifiés peut se faire uniquement à l'aide du
       fichier IDX et du fichier PACK - en effet, les objets ayant une référence
       OBJ_REF_DELTA ont une résolution possible à l'aide du fichier IDX et si
       le fichier PACK est non-/thin/, ces références sont forcément dans le
       fichier PACK.

       Di côté de Git, c'est nécessairement le cas que les fichiers PACK dans le
       dépôt sont non-/thin/ (et c'est pour cette raison qu'on s'applique à
       faire la troisième phase d'ailleurs). Cependant, dans des situations non
       communes à git, il peut arriver que les fichiers PACK soient /thin/. Il
       faut donc utiliser cette fonction en état de cause.

       TODO: on pourrait faire la vérification si le fichier PACK est /thin/ ou
       pas en regardant si, après la première /pass/ on a bien tout les objets
       dans nos ~Hashtbl~.

       Comme pour [make], la politique d'allocation est déterminé mais elle est
       concurrente ! *)
    let force
      (type mmu) (type location)
      ~root
      ~read_and_exclude
      ~ztmp
      ~window
      fs
      (r:(mmu, location) r)
      exists =
      let path = Fpath.(root / "objects" / "pack" / Fmt.strf "pack-%s.pack" (Hash.to_hex exists.Exists.hash_pack)) in
      let stream = stream_of_path path in
      let thin = ref false in
      let idx hash = match IDec.find exists.Exists.index hash with
        | Some _ as v -> v
        | none -> thin := true; none in

      PInfo.first_pass ~ztmp ~window ~idx stream >>= function
      | Ok info ->
        let info = PInfo.resolve ~length:(IDec.cardinal exists.Exists.index) info in
        let `Resolved path_delta = info.PInfo.state in

        let length_hunks = Normalized.length_of_path path_delta in
        let length_objrw = Hashtbl.fold (fun k (_, _, v) -> max v) info.PInfo.index 0 in
        let buff = make_buffer r length_hunks length_objrw path_delta in

        let ( >>!= ) v f = v >>= function Ok _ as v -> Lwt.return v | Error err -> f err in

        Loaded.make_pack_decoder ~read_and_exclude ~idx:Option.(fun hash -> Hashtbl.find_opt info.PInfo.index hash >>= fun (crc, abs_off, _) -> Some (crc, abs_off)) fs path
        >>?= fun (fd, pack) -> FS.Mapper.close exists.Exists.fd >|= Rresult.R.reword_error Error.FS.err_sys_map
        >>?= fun () -> Lwt.return_ok { pack
                                     ; index = info.PInfo.index
                                     ; delta = info.PInfo.delta
                                     ; hash_pack = exists.Exists.hash_pack
                                     ; path_delta
                                     ; fd
                                     ; buff
                                     ; thin = !thin }
        >>!= fun er -> FS.Mapper.close exists.Exists.fd >|= Rresult.R.reword_error Error.FS.err_sys_map
        >>?= fun () -> Lwt.return_error er
      | Error err -> Lwt.return (Error (`Pack_info err))

    let lookup { index; _ } hash = match Hashtbl.find_opt index hash with
      | Some (crc, abs_off, _) -> Some (crc, abs_off)
      | None -> None

    let size { pack; _ } ~ztmp ~window hash =
      RPDec.length pack hash ztmp window >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)

    let read (type value) ~ztmp ~window ~(to_result:RPDec.Object.t -> (value, error) result Lwt.t) ({ pack; thin; _ } as t) hash
      : [ `Error of error | `Promote of value | `Return of value ] Lwt.t =
      with_buffer t.buff @@ fun { hunks; buffer; deliver; _ } ->
      RPDec.get_from_hash ~htmp:hunks pack hash buffer ztmp window >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)
      >>?= to_result >>= fun res -> deliver () >|= fun () -> match res with
      | Ok value ->
        if not thin then `Promote value else `Return value
      | Error err -> ` Error err
  end

  module Total =
  struct
    type t =
      { index      : (Hash.t, Crc32.t * int64 * int) Hashtbl.t
      ; path_delta : PInfo.path
      ; hash_pack  : Hash.t
      ; pack       : RPDec.t
      ; buff       : (Resolved.buffer -> unit Lwt.t) -> unit Lwt.t
      ; fd         : FS.Mapper.fd }

    let lookup { index; _ } hash = Hashtbl.find_opt index hash
    let mem { index; _ } hash = Hashtbl.mem index hash
    let fold f { index; _ } a = Hashtbl.fold (fun hash (crc, abs_off, _) a -> f hash (crc, abs_off) a) index a

    let size { pack; _ } ~ztmp ~window hash =
      RPDec.length pack hash ztmp window >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)

    let read (type value) ~ztmp ~window ~(to_result:RPDec.Object.t -> (value, error) result Lwt.t) ({ pack; _ } as t) hash
      : [ `Error of error | `Return of value ] Lwt.t =
      with_buffer t.buff @@ fun { hunks; buffer; deliver; _ } ->
      RPDec.get_from_hash ~htmp:hunks pack hash buffer ztmp window >|= Rresult.R.reword_error (fun err -> `Pack_decoder err)
      >>?= to_result >>= fun res -> deliver () >|= fun () -> match res with
      | Ok value -> `Return value
      | Error err -> ` Error err

    exception Leave of Hash.t

    module EPACK = struct
      module E = struct
        type state =
          { get : Hash.t -> Cstruct.t option Lwt.t
          ; pack: PEnc.t
          ; src : Cstruct.t option }
        type result =
          { tree: (Crc32.t * int64) PEnc.Map.t
          ; hash: Hash.t }
        type error = PEnc.error

        let option_get ~default = function Some a -> a | None -> default

        let rec eval dst state =
          match
            PEnc.eval (option_get ~default:empty_cstruct state.src) dst state.pack
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

    let store_pack_file ~fs fmt entries get =
      let ztmp = Cstruct.create 0x8000 in
      let file = fmt (random_string 10) in
      let state = PEnc.default ztmp entries in
      FS.Dir.temp fs >>= fun temp ->
      let path = Fpath.(temp / file) in
      let rawo = Cstruct.create 0x8000 in
      let state = { EPACK.E.get; src = None; pack = state } in
      Lwt.catch
        (fun () ->
           (* XXX(dinosaure): why is not atomic? *)
           EPACK.to_file fs ~atomic:false path rawo state >|= function
           | Ok { EPACK.E.tree; hash; } ->
             let index = Hashtbl.create (PEnc.Map.cardinal tree) in
             let paths = Hashtbl.create (PEnc.Map.cardinal tree) in
             List.iter (fun (entry, delta) ->
                 let (crc, abs_off) = PEnc.Map.find (PEnc.Entry.id entry) tree in
                 let path, needed = match delta.PEnc.Delta.delta with
                   | PEnc.Delta.Z ->
                     let length = Int64.to_int (PEnc.Entry.length entry) in
                     PInfo.Load length, length
                   | PEnc.Delta.S { src_length; _ } as delta ->
                     let rec go ~src_length:length k acc = function
                       | PEnc.Delta.S { length; src = { PEnc.Delta.delta }; hunks; src_length; _ } ->
                         let hunks = List.fold_left (fun a -> function Rabin.Insert (_, l) -> a + l | _ -> a) 0 hunks in
                         go ~src_length (fun src -> PInfo.Patch { hunks; target = length; src }) (max acc length) delta
                       | PEnc.Delta.Z -> k (PInfo.Load (Int64.to_int length)), acc in
                     go ~src_length (fun x -> x) 0 delta in
                 Hashtbl.add index hash (crc, abs_off, needed);
                 Hashtbl.add paths abs_off path) entries;

             let rec merge abs_off path acc = match path, acc with
               | PInfo.Load a, PInfo.Load b -> PInfo.Load (max a b)
               | PInfo.Load x, PInfo.Patch { hunks; target; src; }
               | PInfo.Patch { hunks; target; src; }, Load x -> PInfo.Patch { hunks; target = max x target; src; }
               | PInfo.Patch { hunks = hunks_a
                       ; target = target_a
                       ; src = src_a },
                 Patch { hunks = hunks_b
                       ; target = target_b
                       ; src = src_b } ->
                 Patch { hunks = max hunks_a hunks_b; target = max target_a target_b; src = merge abs_off src_a src_b } in
             let path = Hashtbl.fold merge paths (Load 0) in

             Ok (Fpath.(temp / file)
                , (index, path)
                , hash)
           | Error #fs_error as err -> err
           | Error (`Encoder e) -> Error (`Pack_encoder e))
        (function
          | Leave hash -> Lwt.return (Error (`Invalid_hash hash))
          | exn        -> Lwt.fail exn (* XXX(dinosaure): should never happen. *))

    let filter_map f lst =
      List.fold_left (fun a v -> match f v with
          | Some v -> v :: a
          | None -> a) [] lst
      |> List.rev

    let third_pass ~root ~ztmp ~window ~read_inflated fs resolved =
      let deltas = filter_map (function PInfo.Delta { hunks_descr; _ } -> Some hunks_descr | _ -> None)
          (Hashtbl.fold (fun _ v a -> v :: a) resolved.Resolved.delta []) in
      let revidx = Hashtbl.create (Hashtbl.length resolved.Resolved.index) in
      Hashtbl.iter (fun hash (crc, abs_off, length)-> Hashtbl.add revidx abs_off hash) resolved.Resolved.index;

      let k2k = function
        | `Commit -> Pack.Kind.Commit
        | `Tree   -> Pack.Kind.Tree
        | `Blob   -> Pack.Kind.Blob
        | `Tag    -> Pack.Kind.Tag in
      let make acc (hash, (_, abs_off, _)) =
        with_buffer resolved.Resolved.buff @@ fun { hunks; buffer; deliver; _ } ->

        RPDec.get_from_offset ~htmp:hunks resolved.Resolved.pack abs_off buffer ztmp window
        >>= fun obj -> deliver () >|= fun () -> match obj with
        | Error _ -> acc
        | Ok obj ->
          let delta = match obj.RPDec.Object.from with
            | RPDec.Object.External { hash; _ } -> Some (PEnc.Entry.From hash)
            | RPDec.Object.Internal _ -> None
            | RPDec.Object.Delta { offset; _ } ->
              try let hash = Hashtbl.find revidx offset in
                Some (PEnc.Entry.From hash)
              with Not_found -> None (* XXX(dinosaure): should not appear. *) in
          PEnc.Entry.make hash ?delta (k2k obj.RPDec.Object.kind) (Int64.of_int (Cstruct.len obj.RPDec.Object.raw)) :: acc in
      let external_objects acc =
        let res = List.fold_left
            (fun acc hunks_descr ->
               match hunks_descr.HDec.reference with
               | HDec.Offset _ -> acc
               | HDec.Hash hash ->
                 if Hashtbl.mem resolved.Resolved.index hash
                 then acc
                 else
                   try ignore @@ List.find (Hash.equal hash) acc; acc
                   with Not_found -> hash :: acc)
            [] deltas in
        Lwt_list.fold_left_s
          (fun acc hash -> read_inflated hash >|= function
             | None -> acc
             | Some (kind, raw) -> PEnc.Entry.make hash (k2k kind) (Int64.of_int (Cstruct.len raw)) :: acc)
          acc res in
      let read_inflated hash =
        if Hashtbl.mem resolved.Resolved.index hash
        then
          with_buffer resolved.Resolved.buff @@ fun { hunks; buffer = (_, _, length); deliver; _ } ->
          let buffer = Cstruct.create (length * 2) in
          let buffer = Cstruct.sub buffer 0 length, Cstruct.sub buffer length length, length in

          RPDec.get_from_hash ~htmp:hunks resolved.Resolved.pack hash buffer ztmp window
          >>= fun obj -> deliver () >|= fun () -> match obj with
          | Error _ -> None
          | Ok obj -> (Some obj.RPDec.Object.raw)
        else read_inflated hash >|= function
          | Some (_, raw) -> Some raw
          (* XXX(dinosaure): normalement, il devrait y avoir un [cstruct_copy]
             ici puisque [read_inflated] est externe à cette fonction - on y
             contrôle donc pas son allocation. Cependant, plus bas (dans [read])
             cette fonction est le [read_and_exclude] qui alloue bien les
             [Cstruct.t]. Donc dans ce cas, ce serait une allocation d'une
             allocation - ce qui est pas le plus top.

             Bref, attention si on change ce code. *)
          | None -> None in

      Hashtbl.fold (fun hash value acc -> (hash, value) :: acc) resolved.Resolved.index []
      |> Lwt_list.fold_left_s make []
      >>= external_objects
      >>= fun entries -> PEnc.Delta.deltas ~memory:false entries read_inflated (fun _ -> false) 10 50
      >>= function
      | Error err -> Lwt.return_error (`Delta err)
      | Ok entries ->
        store_pack_file ~fs (Fmt.strf "pack-%s.pack") entries read_inflated
        >>= function
        | Error _ as err -> Lwt.return err
        | Ok (path, (index, delta_path), hash_pack) ->
          let sequence f = Hashtbl.iter (fun hash (crc, abs_off, _) -> f (hash, (crc, abs_off))) index in
          Resolved.store_idx_file ~root fs sequence hash_pack >>= function
          | Error _ as err -> Lwt.return err
          | Ok () ->
            let file = Fmt.strf "pack-%s.pack" (Hash.to_hex hash_pack) in
            let dst  = Fpath.(root / "objects" / "pack" / file) in
            FS.File.move fs path dst >|= Rresult.R.reword_error (Error.FS.err_move path dst)
            >>?= fun () -> FS.Mapper.close resolved.Resolved.fd >|= Rresult.R.reword_error Error.FS.err_sys_map
            (* XXX(dinosaure): le fichier PACK temporaire de l'état [resolved]
               est fermé ici ! *)
            >>?= fun () -> Lwt.return_ok (path, hash_pack, index, delta_path)

    let make
      (type mmu) (type location)
      ~root
      ~ztmp
      ~window
      ~read_inflated
      fs
      (r:(mmu, location) r)
      resolved =
      if resolved.Resolved.thin
      then third_pass ~root ~ztmp ~window ~read_inflated fs resolved >>= function
        | Error _ -> assert false
        | Ok (path, hash_pack, index, path_delta) ->

          let idx hash = let open Option in Hashtbl.find_opt index hash >|= fun (crc, abs_off, _) -> (crc, abs_off) in
          Loaded.make_pack_decoder ~read_and_exclude:(fun _ -> Lwt.return_none) ~idx fs path
          >>?= fun (fd, pack) ->

            let length_hunks = Normalized.length_of_path path_delta in
            let length_buffer = Hashtbl.fold (fun k (_, _, v) -> max v) index 0 in
            let buff = Resolved.make_buffer r length_hunks length_buffer path_delta in

            Lwt.return_ok { index; path_delta; hash_pack; pack; buff; fd; }
      else
        let extern _ = Lwt.return_none in

        Lwt.return_ok { pack       = RPDec.update_extern extern resolved.Resolved.pack
                      ; index      = resolved.Resolved.index
                      ; hash_pack  = resolved.Resolved.hash_pack
                      ; path_delta = resolved.Resolved.path_delta
                      ; fd         = resolved.Resolved.fd
                      ; buff       = resolved.Resolved.buff }
  end

  type pack =
    | Exists     of Exists.t
    | Loaded     of Loaded.t
    | Resolved   of Resolved.t
    | Total      of Total.t

  type t =
    { packs : (Hash.t, pack) Hashtbl.t }

  let pp_error ppf = function
    | `Pack_decoder err       -> RPDec.pp_error ppf err
    | `Pack_encoder err       -> PEnc.pp_error ppf err
    | `Pack_info err          -> PInfo.pp_error ppf err
    | `Idx_decoder err        -> IDec.pp_error ppf err
    | `Idx_encoder err        -> IEnc.pp_error ppf err
    | #inf_error as err       -> Error.Inf.pp_error Inflate.pp_error ppf err
    | #fs_error as err        -> Error.FS.pp_error FS.pp_error ppf err
    | #Error.Decoder.t as err -> Error.Decoder.pp_error ppf err
    | #Error.not_found as err -> Error.pp_not_found ppf err
    | `Delta err              -> PEnc.Delta.pp_error ppf err
    | `Invalid_hash hash ->
      Fmt.pf ppf "Unable to load %a, it does not exists in the store"
        Hash.pp hash

  (* XXX(dinosaure): this function could not be exposed. It used in a specific
     context, when a pack decoder requests an external object. This case appear
     only when the pack file is a /thin/-pack. So, we need to allocate some
     buffers to avoid a memory-explosion (if the object is delta-ified) and
     return a /fresh/ raw of the requested object. *)
  let strong_weight_read decoder hash request =
    let ztmp = Cstruct.create 0x8000 in
    let window = Inflate.window () in
    (RPDec.get_with_result_allocation_from_hash decoder request ztmp window >>= function
      | Ok obj -> Lwt.return (Some (obj.RPDec.Object.kind, obj.RPDec.Object.raw))
      | Error err ->
        Log.err (fun l -> l ~header:"read_and_exclude" "Error when we try to get the object %a from the pack %a: %a."
                    Hash.pp request Hash.pp hash
                    RPDec.pp_error err);
        Lwt.return None)

  (* XXX(dinosaure): [read_and_exclude] is called when a pack decoder wants to
     reconstruct an internal object and the source is external - by this way, we
     can consider the pack file as a /thin/ pack.

     So this is the biggest function which could allocate a lot. Obviously, in a
     real world, this function could not happen (Git takes care about pack file
     and stores only /non-thin/ pack files).

     However, from a [resolved]/[normalized]/[loaded]/[exists] information, we
     can not ensure than the pack file is not a /thin/-pack. So this function
     exists for the pack decoder when we want to retrieve an external object.

     Then, we exclude the pack itself to avoid a infinite recursion when the
     length of the redirection is 1. If it's upper, firstly, what? then, you
     could have a infinite loop. About this bug, we could change the API of the
     pack decoder and allow to inform a /close/ list to the function.

     NOTE: [exclude] is added for each call (we put the current hash of the PACK
     file), it's like a close list of already visited PACK files. This is to
     avoid an infinite recursion. It's common to have 2 times (or more) the same
     objet on multiple PACK file.

     So, a /thin/ PACK file A can expect an external object O which appear on a
     PACK file B which need an object M which could be appear inner A (and need
     O to be reconstructed). In other words, O needs itself to be reconstructed.
     This situation appear only if we did not exclude PACK file which we already
     try to get the O object.

     As I said, the object O can be appear 2 times (or more) in multiple PACK
     file. That means, object O can appear in an other PACK file. The goal is to
     get O from a different PACK file than A (may be a PACK file C) and this the
     purpose of [exclude]. My brain is fuck up. *)

  exception Catch of (RPDec.kind * Cstruct.t)

  let rec read_and_exclude ~root ~read_loose fs t exclude request =
    Lwt.catch
      (fun () -> Lwt_list.fold_left_s (fun acc (hash, pack) -> match acc with
           | Some _ -> assert false
           | None -> match pack with
             | Exists exists ->
               if Exists.mem exists request
               then
                 Loaded.make ~root ~read_and_exclude:(read_and_exclude ~root ~read_loose fs t [ hash ]) fs exists >>= function
                 | Error err ->
                   Log.err (fun l -> l "Retrieve an error when we promote PACK %a to loaded state: %a."
                               Hash.pp exists.Exists.hash_pack pp_error err);
                   Lwt.return_none
                 | Ok loaded ->
                   Hashtbl.replace t.packs hash (Loaded loaded);
                   let exclude = hash :: exclude in
                   let read_and_exclude = read_and_exclude ~root ~read_loose fs t exclude in
                   strong_weight_read (RPDec.update_extern read_and_exclude loaded.Loaded.pack) hash request >>= function
                   | Some value -> Lwt.fail (Catch value)
                   | None -> Lwt.return_none
               else Lwt.return_none
             | Loaded loaded ->
               if Loaded.mem loaded request
               then
                 let exclude = hash :: exclude in
                 let read_and_exclude = read_and_exclude ~root ~read_loose fs t exclude in
                 strong_weight_read (RPDec.update_extern read_and_exclude loaded.Loaded.pack) hash request >>= function
                 | Some value -> Lwt.fail (Catch value)
                 | None -> Lwt.return_none
               else Lwt.return_none
             | Resolved resolved ->
               if Resolved.mem resolved request
               then
                 let exclude = hash :: exclude in
                 let read_and_exclude = read_and_exclude ~root ~read_loose fs t exclude in
                 strong_weight_read (RPDec.update_extern read_and_exclude resolved.Resolved.pack) hash request >>= function
                 | Some value -> Lwt.fail (Catch value)
                 | None -> Lwt.return_none
               else Lwt.return_none
             | Total total ->
               if Total.mem total request
               then
                 let exclude = hash :: exclude in
                 let read_and_exclude = read_and_exclude ~root ~read_loose fs t exclude in
                 strong_weight_read (RPDec.update_extern read_and_exclude total.Total.pack) hash request >>= function
                 | Some value -> Lwt.fail (Catch value)
                 | None -> Lwt.return_none
               else Lwt.return_none)
           None
           (Hashtbl.fold (fun hash pack acc ->
                if List.exists (Hash.equal hash) exclude
                then acc
                else (hash, pack) :: acc) t.packs []))
      (function
        | Catch (kind, raw) -> Lwt.return_some (kind, raw)
        | exn -> Lwt.fail exn)
    >>= function
    | None -> read_loose request
    | Some x -> Lwt.return_some x

  let v fs indexes =
    Lwt_list.map_p (Exists.make fs) indexes >|= fun exists ->
    let packs = Hashtbl.create 32 in
    List.iter (function Ok ({ Exists.hash_pack; _ } as exists) -> Hashtbl.add packs hash_pack (Exists exists)
                      | Error err ->
                        Log.err (fun l -> l "Retrieve an error when we load IDX file: %a." pp_error err))
      exists;
    { packs }

  let add ~root ~read_loose ~ztmp ~window fs r t path_tmp info =
    let read_and_exclude = read_and_exclude ~root ~read_loose fs t [ info.PInfo.hash_pack ] in
    let read_inflated = read_and_exclude in

    Normalized.make_from_info ~read_and_exclude fs path_tmp info
    >>?= Resolved.make_from_normalized ~root ~read_and_exclude ~ztmp ~window fs r
    >>?= Total.make ~root ~ztmp ~window ~read_inflated fs r
    >>= function
    | Ok total ->
      Hashtbl.add t.packs total.Total.hash_pack (Total total);
      Lwt.return_ok ()
    | Error _ as err -> Lwt.return err

  exception Found of (Hash.t * (Crc32.t * int64))

  let lookup t hash =
    let jump0 hash = function
      | Some (crc, abs_off) -> raise (Found (hash, (crc, abs_off)))
      | None -> () in
    let jump1 hash = function
      | Some (crc, abs_off, _) -> raise (Found (hash, (crc, abs_off)))
      | None -> () in
    try
      Hashtbl.iter
        (fun hash_pack -> function
           | Exists exists -> Exists.lookup exists hash |> jump0 hash_pack
           | Loaded loaded -> Loaded.lookup loaded hash |> jump0 hash_pack
           | Resolved resolved -> Resolved.lookup resolved hash |> jump0 hash_pack
           | Total total -> Total.lookup total hash |> jump1 hash_pack)
        t.packs; None
    with Found v -> Some v

  let mem t hash =
      Hashtbl.fold (fun hash_pack -> function
          | Exists exists -> (||) (Exists.mem exists hash)
          | Loaded loaded -> (||) (Loaded.mem loaded hash)
          | Resolved resolved -> (||) (Resolved.mem resolved hash)
          | Total total -> (||) (Total.mem total hash))
        t.packs false

  let mem t hash =
    lookup t hash |> function
    | Some _ -> true
    | None   -> false

  let read ~root ~read_loose ~to_result ~ztmp ~window fs r t hash =
    let read_and_exclude hash_pack = read_and_exclude ~root ~read_loose fs t [ hash_pack ] in
    let read_inflated hash_pack = read_and_exclude hash_pack in

    (* XXX(dinosaure): [read_inflated] pourrait être optimiser. Son contexte
       d'utilisation concerne la troisième /pass/ où un commentaire signale
       l'utilisation explicite de [read_and_exclude] en lieu et place d'une
       fonction plus optimisé (à propos de l'allocation). *)

    let promote_loaded fs r (hash_pack, loaded) obj =
      Resolved.make_from_loaded ~read_and_exclude:(read_and_exclude hash_pack) ~ztmp ~window fs r loaded >>= function
      | Ok resolved ->
        Hashtbl.replace t.packs hash_pack (Resolved resolved);
        Lwt.return_ok obj
      | Error _ as err -> Lwt.return err in

    let return_loaded fs r loaded = function
      | `Return ret -> Lwt.return_ok ret
      | `Error err -> Lwt.return_error err
      | `Promote obj -> promote_loaded fs r loaded obj in

    let promote_resolved fs r (hash_pack, resolved) obj =
      Total.make ~root ~ztmp ~window ~read_inflated:(read_inflated hash_pack) fs r resolved >>= function
      | Ok total ->
        Hashtbl.replace t.packs hash_pack (Total total);
        Lwt.return_ok obj
      | Error _ as err -> Lwt.return err in

    let return_resolved fs r resolved = function
      | `Return ret -> Lwt.return_ok ret
      | `Error err -> Lwt.return_error err
      | `Promote obj -> promote_resolved fs r resolved obj in

    lookup t hash |> function
    | None -> Lwt.return_error `Not_found
    | Some (hash_pack, _) ->
      match Hashtbl.find t.packs hash_pack with
      | Loaded loaded ->
        Loaded.read ~root r ~to_result ~ztmp ~window loaded hash
        >>= return_loaded fs r (hash_pack, loaded)
      | Exists exists ->
        (Loaded.make ~root ~read_and_exclude:(read_and_exclude exists.Exists.hash_pack) fs exists >>= function
          | Error _ as err -> Lwt.return err
          | Ok loaded ->
            Hashtbl.replace t.packs hash_pack (Loaded loaded);
            Loaded.read ~root r ~to_result ~ztmp ~window loaded hash
            >>= return_loaded fs r (hash_pack, loaded))
      | Resolved resolved ->
        Resolved.read ~ztmp ~window ~to_result resolved hash >>= return_resolved fs r (hash_pack, resolved)
      | Total total ->
        Total.read ~ztmp ~window ~to_result total hash >>= function
        | `Error err -> Lwt.return_error err
        | `Return obj -> Lwt.return_ok obj

  let size ~root ~read_loose ~ztmp ~window fs t hash =
    let read_and_exclude hash_pack = read_and_exclude ~root ~read_loose fs t [ hash_pack ] in

    lookup t hash |> function
    | None -> Lwt.return_error `Not_found
    | Some (hash_pack, _) ->
      match Hashtbl.find t.packs hash_pack with
      | Exists exists ->
        (Loaded.make ~root ~read_and_exclude:(read_and_exclude exists.Exists.hash_pack) fs exists >>= function
          | Error _ as err -> Lwt.return err
          | Ok loaded ->
            Hashtbl.replace t.packs hash_pack (Loaded loaded);
            Loaded.size loaded ~ztmp ~window hash)
      | Loaded loaded -> Loaded.size loaded ~ztmp ~window hash
      | Resolved resolved -> Resolved.size resolved ~ztmp ~window hash
      | Total total -> Total.size total ~ztmp ~window hash

  let fold f t a =
    Hashtbl.fold (fun hash_pack pack acc -> match pack with
        | Exists exists -> Exists.fold f exists acc
        | Loaded loaded -> Loaded.fold f loaded acc
        | Resolved resolved -> Resolved.fold f resolved acc
        | Total total -> Total.fold f total acc)
      t.packs a

  let list t = fold (fun hash _ acc ->
      if List.exists (Hash.equal hash) acc
      then acc else hash :: acc)
      t []
end
