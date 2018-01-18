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

let src = Logs.Src.create "git.loose" ~doc:"logs git's loose event"
module Log = (val Logs.src_log src : Logs.LOG)

let error e fmt = Fmt.kstrf (fun x ->
    Log.err (fun l -> l "%s" x);
    Lwt.return (Error e)
  ) fmt

module type S = sig

  module FS: S.FS
  module Value: Value.S
  include module type of Value

  type error =
    [ Error.Decoder.t2
    | Error.Angstrom.t2
    | `Inflate_file of Fpath.t * Inflate.error
    | `Deflate_file of Fpath.t * Deflate.error
    | `Move   of Fpath.t * Fpath.t * FS.error
    | `Stack  of Fpath.t
    | `Length of Fpath.t * FS.error
    | `Map    of Fpath.t * FS.error
    | `Create of Fpath.t * FS.error
    | `Delete of Fpath.t * FS.error
    | `Close  of Fpath.t * FS.error
    | `Open   of Fpath.t * FS.error
    | `Read   of Fpath.t * FS.error
    | `Write  of Fpath.t * FS.error ]

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  val pp_error : error Fmt.t

  val mem:
    root:Fpath.t ->
    Hash.t -> bool Lwt.t

  val read:
    root:Fpath.t ->
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    Hash.t -> (t, error) result Lwt.t

  val inflate:
    root:Fpath.t ->
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    Hash.t -> (kind * Cstruct.t, error) result Lwt.t

  val inflate_wa:
    root:Fpath.t ->
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    result:Cstruct.t ->
    Hash.t -> (kind * Cstruct.t, error) result Lwt.t

  val list:
    root:Fpath.t ->
    Hash.t list Lwt.t

  val size:
    root:Fpath.t ->
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    Hash.t -> (int64, error) result Lwt.t

  val write:
    root:Fpath.t ->
    ?capacity:int ->
    ?level:int ->
    ztmp:Cstruct.t ->
    raw:Cstruct.t ->
    t -> (Hash.t * int, error) result Lwt.t

  val write_inflated:
    root:Fpath.t ->
    ?level:int ->
    raw:Cstruct.t ->
    kind:kind ->
    Cstruct.t -> (Hash.t, error) result Lwt.t
end

module Make
    (H: S.HASH)
    (FS: S.FS)
    (I: S.INFLATE)
    (D: S.DEFLATE)
  : S with module Hash = H
       and module Inflate = I
       and module Deflate = D
       and module FS = FS
       and module Blob = Blob.Make(H)
       and module Commit = Commit.Make(H)
       and module Tree = Tree.Make(H)
       and module Tag = Tag.Make(H)
       and type t = Value.Make(H)(I)(D).t
= struct

  module FS = Helper.FS(FS)
  module ErrInf = Error.Inf(I)
  module ErrDef = Error.Def(D)
  module ErrFS  = Error.FS(FS)

  module Value = Value.Make(H)(I)(D)
  include Value

  type error =
    [ ErrFS.t2
    | ErrFS.t3
    | Error.Angstrom.t2
    | Error.Decoder.t2
    | ErrInf.t1
    | ErrDef.t1 ]

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  let pp_error ppf = function
    | #Error.Decoder.t2 as err -> Error.Decoder.t2_pp_error ppf err
    | #ErrInf.t1 as err -> ErrInf.t1_pp_error ppf err
    | #ErrDef.t1 as err -> ErrDef.t1_pp_error ppf err
    | #ErrFS.t2 as err -> ErrFS.t2_pp_error ppf err
    | #ErrFS.t3 as err -> ErrFS.t3_pp_error ppf err

  let hash_get : Hash.t -> int -> int = fun h i -> Char.code @@ Hash.get h i

  let explode hash =
    Fmt.strf "%02x" (hash_get hash 0),
    let buf = Buffer.create ((Hash.Digest.length - 1) * 2) in
    let ppf = Fmt.with_buffer buf in
    for i = 1 to Hash.Digest.length - 1
    do Fmt.pf ppf "%02x%!" (hash_get hash i) done;
    Buffer.contents buf

  let mem ~root hash =
    let first, rest = explode hash in
    let path = Fpath.(root / "objects" / first / rest) in
    Log.debug (fun l ->
        l "Checking if the object %a is a loose file (%a)."
          Hash.pp hash Fpath.pp path);
    FS.File.exists path >|= function
    | Ok v    -> v
    | Error _ -> false

  (* XXX(dinosaure): make this function more resilient: if [of_hex]
     fails), avoid the path. *)
  let list ~root =
    let path = Fpath.(root / "objects") in
    FS.Dir.contents ~rel:true path >>= function
    | Error err ->
      Log.err (fun l ->
          l "Got an error while listing the contents of %a: %a"
            Fpath.pp path FS.pp_error err);
      Lwt.return []
    | Ok firsts ->
      Lwt_list.fold_left_s (fun acc first ->
          FS.Dir.contents ~rel:true Fpath.(path //first) >|= function
          | Ok paths ->
            List.fold_left (fun acc path ->
                let hash = Fpath.(to_string first ^ to_string path) in
                try Hash.of_hex hash :: acc
                with _e ->
                  (* XXX(samoht): avoid catch-all *)
                  Log.warn (fun l ->
                      l "Retrieving a malformed file: %s / %s."
                        (Fpath.to_string first) (Fpath.to_string path));
                  acc
              ) acc paths
          | Error _ -> acc)
        [] firsts

  type 't decoder =
    (module S.DECODER
      with type t = 't
       and type init = Inflate.window * Cstruct.t * Cstruct.t
       and type error = [ Error.Decoder.t0
                        | ErrInf.t0 ])

  let gen (type t) ~root ~window ~ztmp ~dtmp ~raw (decoder : t decoder) hash =
    let module D = (val decoder) in
    let first, rest = explode hash in
    let decoder     = D.default (window, ztmp, dtmp) in
    let path        = Fpath.(root / "objects" / first / rest) in
    Log.debug (fun l -> l "Reading the loose object %a." Fpath.pp path);
    FS.with_open_r path @@ fun ic ->
    let rec loop decoder =
      match D.eval decoder with
      | `Error (_, (#Error.Angstrom.t0 as err)) ->
        Lwt.return Error.(v @@ Error.Angstrom.t2_of_t0 path err)
      | `Error (_, (#Error.Decoder.t0 as err)) ->
        Lwt.return Error.(v @@ Error.Decoder.t2_of_t0 path err)
      | `Error (_, (#ErrInf.t0 as err)) ->
        Lwt.return Error.(v @@ ErrInf.t1_of_t0 path err)
      | `End (_, value) -> Lwt.return (Ok value)
      | `Await decoder ->
        FS.File.read raw ic >>= function
        | Error err -> Lwt.return Error.(v @@ ErrFS.err_read path err)
        | Ok n ->
          Log.debug (fun l ->
              l "Reading %d byte(s) of the file-descriptor (object: %a)."
                n Hash.pp hash);
          match D.refill (Cstruct.sub raw 0 n) decoder with
          | Ok decoder              -> loop decoder
          | Error (#Error.Angstrom.t0 as err) ->
            Lwt.return Error.(v @@ Angstrom.t2_of_t0 path err)
          | Error (#Error.Decoder.t0 as err) ->
            Lwt.return Error.(v @@ Decoder.t2_of_t0 path err)
          | Error (#ErrInf.t0 as err) ->
            Lwt.return Error.(v @@ ErrInf.t1_of_t0 path err)
    in
    loop decoder >|= fun ret ->
    Log.debug (fun l -> l "Finished to read the object %s / %s." first rest);
    ret

  let read ~root ~window ~ztmp ~dtmp ~raw hash =
    gen ~root ~window ~ztmp ~dtmp ~raw (module D) hash

  module HeaderAndBody = struct
    type t = [ `Commit | `Blob | `Tag | `Tree ] * Cstruct.t
    let kind = Value.A.kind
    let int64 = Value.A.length
    let to_end cs =
      let open Angstrom in
      let pos = ref 0 in
      fix @@ fun m ->
      available >>= function
      | 0 ->
        (peek_char >>= function
          | Some _ -> m
          | None   -> commit *> return (Cstruct.sub cs 0 !pos))
      | n -> take n >>= fun chunk ->
        (* XXX(dinosaure): this code [blit] only what is possible to
           copy to [cs]. It can be happen than we don't store all of
           the git object in [cs] but in specific context (when we
           want to decode a source of a delta-ification), this is what
           we want, store only what is needed and limit the memory
           consumption.

           This code is close to the [~result] argument of [decoder]
           and, in fact, if we don't want to store the git object in a
           specific user defined buffer, we ensure to allocate what is
           needed to store all of the git object. *)
        let n' = min n (Cstruct.len cs - !pos) in
        Cstruct.blit_from_string chunk 0 cs !pos n';
        pos := !pos + n;
        commit *> (if n = 0 then return cs else m)

    let sp = ' '
    let nl = '\000'

    let decoder ~result =
      let open Angstrom in
      kind <* char sp
      >>= fun kind -> int64 <* char nl <* commit
      >>= fun length ->
      (match result with
       | Some result -> to_end result
       | None -> to_end (Cstruct.create (Int64.to_int length)))
      >>| fun cs -> kind, cs
  end

  module I = Helper.MakeInflater(Inflate)(struct
      include HeaderAndBody
      let decoder = decoder ~result:None
    end)

  let inflate ~root ~window ~ztmp ~dtmp ~raw hash =
    gen ~root ~window ~ztmp ~dtmp ~raw (module I) hash

  let inflate_wa ~root ~window ~ztmp ~dtmp ~raw ~result hash =
    let module P = Helper.MakeInflater(Inflate)(struct
        include HeaderAndBody
        let decoder = decoder ~result:(Some result)
      end)
    in
    gen ~root ~window ~ztmp ~dtmp ~raw (module P) hash

  module HeaderOnly = struct
    type t = [ `Commit | `Blob | `Tag | `Tree ] * int64
    let kind = HeaderAndBody.kind
    let int64 = HeaderAndBody.int64
    let decoder =
      let open Angstrom in
      kind <* take 1
      >>= fun kind -> int64 <* advance 1
      >>| fun length -> kind, length
  end

  module S = Helper.MakeInflater(Inflate)(HeaderOnly)

  let size ~root ~window ~ztmp ~dtmp ~raw hash =
    let first, rest = explode hash in
    let decoder     = S.default (window, ztmp, dtmp) in
    let path = Fpath.(root / "objects" / first / rest) in
    Log.debug (fun l -> l "Reading the loose object %a." Fpath.pp path);
    FS.with_open_r path @@ fun read ->
    let rec loop decoder = match S.eval decoder with
      | `Error (_, (#Error.Angstrom.t0 as err)) ->
        Lwt.return Error.(v @@ Error.Angstrom.t2_of_t0 path err)
      | `Error (_, (#Error.Decoder.t0 as err)) ->
        Lwt.return Error.(v @@ Error.Decoder.t2_of_t0 path err)
      | `Error (_, (#ErrInf.t0 as err)) ->
        Lwt.return Error.(v @@ ErrInf.t1_of_t0 path err)
      | `End (_, (_, size))           -> Lwt.return (Ok size)
      | `Await decoder                ->
        FS.File.read raw read >>= function
        | Error err -> Lwt.return Error.(v @@ ErrFS.err_read path err)
        | Ok n      ->
          match S.refill (Cstruct.sub raw 0 n) decoder with
          | Ok decoder              -> loop decoder
          | Error (#Error.Angstrom.t0 as err) ->
            Lwt.return Error.(v @@ Angstrom.t2_of_t0 path err)
          | Error (#Error.Decoder.t0 as err) ->
            Lwt.return Error.(v @@ Decoder.t2_of_t0 path err)
          | Error (#ErrInf.t0 as err) ->
            Lwt.return Error.(v @@ ErrInf.t1_of_t0 path err)
    in
    loop decoder

  module EDeflated = struct
    module E = struct
      type state  = {
        state: Deflate.t;
        v    : Cstruct.t;
      }
      type result = unit
      type error  = Deflate.error
      let rec eval raw { state; v } =
        match Deflate.eval ~src:v ~dst:raw state with
        | `Await state          -> eval raw {v; state = Deflate.finish state}
        | `Flush state          -> Lwt.return (`Flush {v; state})
        | `Error (state, error) -> Lwt.return (`Error ( {v; state}, error))
        | `End state            -> Lwt.return (`End ({v; state}, ()))
      let used t = Deflate.used_out t.state
      let flush x y {v; state} = {v; state = Deflate.flush x y state}
    end
    include Helper.Encoder(E)(FS)
  end

  let write_inflated ~root ?(level = 4) ~raw ~kind value =
    let header =
      Fmt.kstrf Cstruct.of_string "%s %d\000%!"
        (match kind with
         | `Commit -> "commit"
         | `Blob -> "blob"
         | `Tree -> "tree"
         | `Tag -> "tag")
        (Cstruct.len value)
    in
    let digest value' =
      let ctx = Hash.Digest.init () in
      Hash.Digest.feed ctx value';
      Hash.Digest.get ctx
    in
    let value' = Cstruct.concat [ header; value ] in
    let state = {
      EDeflated.E.v = value';
      state = Deflate.no_flush 0 (Cstruct.len value') (Deflate.default level)
    } in
    let hash = digest value' in
    let first, rest = explode hash in
    let path = Fpath.(root / "objects" / first) in
    FS.Dir.create path >>= function
    | Error err         ->
      Lwt.return Error.(v @@ ErrFS.err_create path err)
    | Ok (true | false) ->
      let path = Fpath.(path / rest) in
      EDeflated.to_file path raw state >|= function
      | Ok ()                -> Ok hash
      | Error #ErrFS.t2 as err -> err
      | Error #ErrFS.t3 as err -> err
      | Error (`Encoder err)   -> Error.(v @@ ErrDef.err_deflate_file path err)

  module EInflated = struct
    module E = struct
      type state  = E.encoder
      type result = int
      type error  = E.error
      let used = E.used
      let flush = E.flush
      let eval raw state =
        match E.eval raw state with
        | `Flush state -> Lwt.return (`Flush state)
        | `Error error -> Lwt.return (`Error (state, error))
        | `End state   -> Lwt.return (`End state)
    end
    include Helper.Encoder(E)(FS)
  end

  let write ~root ?(capacity = 0x100) ?(level = 4) ~ztmp ~raw value =
    let hash        = digest value in
    let first, rest = explode hash in
    let encoder     = E.default (capacity, value, level, ztmp) in
    let path        = Fpath.(root / "objects" / first / rest) in
    Log.debug (fun l -> l "Writing a new loose object %a." Fpath.pp path);
    FS.Dir.create Fpath.(root / "objects" / first) >>= function
    | Error err ->
      Lwt.return Error.(v @@ ErrFS.err_create path err)
    | Ok (true | false) ->
      EInflated.to_file path raw encoder >|= function
      | Error #ErrFS.t2 as err               -> err
      | Error #ErrFS.t3 as err               -> err
      | Error (`Encoder (#ErrDef.t0 as err)) -> Error.(v @@ ErrDef.t1_of_t0 path err)
      | Ok r ->
        Log.debug (fun l -> l "Wrote the object %s/%s" first rest);
        Ok (hash, r)

end
