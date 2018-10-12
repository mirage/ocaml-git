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

module type S = sig
  module FS : S.FS
  module Hash : S.HASH
  module Deflate : S.DEFLATE
  module Inflate : S.INFLATE

  module Value :
    Value.S
    with module Hash := Hash
     and module Deflate := Deflate
     and module Inflate := Inflate

  include module type of Value

  type error =
    [ Error.Decoder.t
    | Inflate.error Error.Inf.t
    | Deflate.error Error.Def.t
    | FS.error Error.FS.t ]

  type kind = [`Commit | `Tree | `Tag | `Blob]

  val pp_error : error Fmt.t
  val mem : fs:FS.t -> root:Fpath.t -> Hash.t -> bool Lwt.t

  val read :
       fs:FS.t
    -> root:Fpath.t
    -> window:Inflate.window
    -> ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> Hash.t
    -> (t, error) result Lwt.t

  val read_inflated :
       fs:FS.t
    -> root:Fpath.t
    -> window:Inflate.window
    -> ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> Hash.t
    -> (kind * Cstruct.t, error) result Lwt.t

  val read_inflated_without_allocation :
       fs:FS.t
    -> root:Fpath.t
    -> window:Inflate.window
    -> ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> result:Cstruct.t
    -> Hash.t
    -> (kind * Cstruct.t, error) result Lwt.t

  val list : fs:FS.t -> root:Fpath.t -> Hash.t list Lwt.t

  val size :
       fs:FS.t
    -> root:Fpath.t
    -> window:Inflate.window
    -> ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> Hash.t
    -> (int64, error) result Lwt.t

  val write :
       fs:FS.t
    -> root:Fpath.t
    -> temp_dir:Fpath.t
    -> ?capacity:int
    -> ?level:int
    -> ztmp:Cstruct.t
    -> raw:Cstruct.t
    -> t
    -> (Hash.t * int, error) result Lwt.t

  val write_deflated :
       fs:FS.t
    -> root:Fpath.t
    -> temp_dir:Fpath.t
    -> ?level:int
    -> raw:Cstruct.t
    -> kind:kind
    -> Cstruct.t
    -> (Hash.t, error) result Lwt.t
end

module Make
    (Hash : S.HASH)
    (FS : S.FS)
    (Inflate : S.INFLATE)
    (Deflate : S.DEFLATE) =
struct
  module FS = Helper.FS (FS)
  module Value = Value.Make (Hash) (Inflate) (Deflate)
  include Value

  type inf_error = Inflate.error Error.Inf.t
  type def_error = Deflate.error Error.Def.t
  type fs_error = FS.error Error.FS.t
  type error = [fs_error | Error.Decoder.t | inf_error | def_error]
  type kind = [`Commit | `Tree | `Tag | `Blob]

  let pp_error ppf = function
    | #Error.Decoder.t as err -> Error.Decoder.pp_error ppf err
    | #inf_error as err -> Error.Inf.pp_error Inflate.pp_error ppf err
    | #def_error as err -> Error.Def.pp_error Deflate.pp_error ppf err
    | #fs_error as err -> Error.FS.pp_error FS.pp_error ppf err

  let explode hash =
    ( Fmt.strf "%02x" (Hash.read hash 0)
    , let buf = Buffer.create ((Hash.digest_size - 1) * 2) in
      let ppf = Fmt.with_buffer buf in
      for i = 1 to Hash.digest_size - 1 do
        Fmt.pf ppf "%02x%!" (Hash.read hash i)
      done ;
      Buffer.contents buf )

  let mem ~fs ~root:dotgit hash =
    let first, rest = explode hash in
    let path = Fpath.(dotgit / "objects" / first / rest) in
    Log.debug (fun l ->
        l "Checking if the object %a is a loose file (%a)." Hash.pp hash
          Fpath.pp path ) ;
    FS.File.exists fs path >|= function Ok v -> v | Error _ -> false

  (* XXX(dinosaure): make this function more resilient: if [of_hex] fails),
     avoid the path. *)
  let list ~fs ~root:dotgit =
    let path = Fpath.(dotgit / "objects") in
    FS.Dir.contents fs ~rel:true path
    >>= function
    | Error err ->
        Log.err (fun l ->
            l "Got an error while listing the contents of %a: %a" Fpath.pp path
              FS.pp_error err ) ;
        Lwt.return []
    | Ok firsts ->
        Lwt_list.fold_left_s
          (fun acc first ->
            FS.Dir.contents fs ~rel:true Fpath.(path // first)
            >|= function
            | Ok paths ->
                List.fold_left
                  (fun acc path ->
                    let hash = Fpath.(to_string first ^ to_string path) in
                    try Hash.of_hex hash :: acc with _e ->
                      (* XXX(samoht): avoid catch-all *)
                      Log.warn (fun l ->
                          l "Retrieving a malformed file: %s / %s."
                            (Fpath.to_string first) (Fpath.to_string path) ) ;
                      acc )
                  acc paths
            | Error _ -> acc )
          [] firsts

  type ('result, 'decoder) decoder =
    (module
     Helper.DECODER
       with type t = 'result
        and type decoder = 'decoder
        and type error = [Error.Decoder.t | `Inflate of Inflate.error])

  let gen (type state result) ~fs ~root:dotgit (state : state) ~raw
      (decoder : (result, state) decoder) hash =
    let module D = (val decoder) in
    let module Decoder = Helper.Decoder (D) (FS) in
    let first, rest = explode hash in
    let file = Fpath.(dotgit / "objects" / first / rest) in
    Log.debug (fun l -> l "Reading the loose object %a." Fpath.pp file) ;
    Decoder.of_file fs file raw state
    >|= function
    | Ok _ as v -> v
    | Error (`Decoder (#Error.Decoder.t as err)) ->
        Error.(v @@ Error.Decoder.with_path file err)
    | Error (`Decoder (`Inflate _ as err)) ->
        Error.(v @@ Inf.with_path file err)
    | Error #fs_error as err -> err

  let read ~fs ~root ~window ~ztmp ~dtmp ~raw hash =
    let state = D.default (window, ztmp, dtmp) in
    gen ~fs ~root state ~raw (module D) hash

  module HeaderAndBody = struct
    type e = [`Commit | `Blob | `Tag | `Tree] * Cstruct.t
    type 'a t = 'a Angstrom.t

    let kind = Value.A.kind
    let int64 = Value.A.length

    let to_end cs =
      let open Angstrom in
      let pos = ref 0 in
      fix
      @@ fun m ->
      available
      >>= function
      | 0 -> (
          peek_char
          >>= function
          | Some _ -> m | None -> commit *> return (Cstruct.sub cs 0 !pos) )
      | n ->
          take n
          >>= fun chunk ->
          (* XXX(dinosaure): this code [blit] only what is possible to copy to
             [cs]. It can be happen than we don't store all of the git object
             in [cs] but in specific context (when we want to decode a source
             of a delta-ification), this is what we want, store only what is
             needed and limit the memory consumption.

             This code is close to the [~result] argument of [decoder] and, in
             fact, if we don't want to store the git object in a specific user
             defined buffer, we ensure to allocate what is needed to store all
             of the git object. *)
          let n' = min n (Cstruct.len cs - !pos) in
          Cstruct.blit_from_string chunk 0 cs !pos n' ;
          pos := !pos + n ;
          commit *> if n = 0 then return cs else m

    let sp = ' '
    let nl = '\000'

    let decoder ~result =
      let open Angstrom in
      kind
      <* char sp
      >>= fun kind ->
      int64
      <* char nl
      <* commit
      >>= fun length ->
      ( match result with
      | Some result -> to_end result
      | None -> to_end (Cstruct.create (Int64.to_int length)) )
      >>| fun cs -> kind, cs
  end

  module I =
    Helper.MakeInflater
      (Inflate)
      (struct
        include HeaderAndBody

        let p = decoder ~result:None
      end)

  let read_inflated ~fs ~root ~window ~ztmp ~dtmp ~raw hash =
    let state = I.default (window, ztmp, dtmp) in
    gen ~fs ~root state ~raw (module I) hash

  let read_inflated_without_allocation ~fs ~root ~window ~ztmp ~dtmp ~raw
      ~result hash =
    let module P =
      Helper.MakeInflater
        (Inflate)
        (struct
          include HeaderAndBody

          let p = decoder ~result:(Some result)
        end)
    in
    let state = P.default (window, ztmp, dtmp) in
    gen ~fs ~root state ~raw (module P) hash

  module HeaderOnly = struct
    type e = [`Commit | `Blob | `Tag | `Tree] * int64
    type 'a t = 'a Angstrom.t

    let kind = HeaderAndBody.kind
    let int64 = HeaderAndBody.int64

    let p =
      let open Angstrom in
      kind
      <* take 1
      >>= fun kind -> int64 <* advance 1 >>| fun length -> kind, length
  end

  module S = Helper.MakeInflater (Inflate) (HeaderOnly)

  let size ~fs ~root ~window ~ztmp ~dtmp ~raw hash =
    let state = S.default (window, ztmp, dtmp) in
    gen ~fs ~root state ~raw (module S) hash
    >|= function Ok (_, v) -> Ok v | Error _ as err -> err

  module EDeflated = struct
    module E = struct
      type state = {state: Deflate.t; v: Cstruct.t}
      type result = unit
      type error = Deflate.error

      let rec eval raw {state; v} =
        match Deflate.eval ~src:v ~dst:raw state with
        | `Await state -> eval raw {v; state= Deflate.finish state}
        | `Flush state -> Lwt.return (`Flush {v; state})
        | `Error (state, error) -> Lwt.return (`Error ({v; state}, error))
        | `End state -> Lwt.return (`End ({v; state}, ()))

      let used t = Deflate.used_out t.state
      let flush x y {v; state} = {v; state= Deflate.flush x y state}
    end

    include Helper.Encoder (E) (FS)
  end

  let write_deflated ~fs ~root:dotgit ~temp_dir ?(level = 4) ~raw ~kind value =
    let header =
      Fmt.kstrf Cstruct.of_string "%s %d\000%!"
        ( match kind with
        | `Commit -> "commit"
        | `Blob -> "blob"
        | `Tree -> "tree"
        | `Tag -> "tag" )
        (Cstruct.len value)
    in
    let digest value' =
      let ctx = Hash.init () in
      let ctx = Hash.feed_bigstring ctx (Cstruct.to_bigarray value') in
      Hash.get ctx
    in
    let value' = Cstruct.concat [header; value] in
    let state =
      { EDeflated.E.v= value'
      ; state= Deflate.no_flush 0 (Cstruct.len value') (Deflate.default level)
      }
    in
    let hash = digest value' in
    let first, rest = explode hash in
    let path = Fpath.(dotgit / "objects" / first) in
    FS.Dir.create fs path
    >>= function
    | Error err -> Lwt.return Error.(v @@ FS.err_create path err)
    | Ok (true | false) -> (
        let path = Fpath.(path / rest) in
        EDeflated.to_file fs ~temp_dir path raw state
        >|= function
        | Ok () -> Ok hash
        | Error #fs_error as err -> err
        | Error (`Encoder err) -> Error.(v @@ Def.err_deflate_file path err) )

  module EInflated = struct
    module E = struct
      type state = E.encoder
      type result = int
      type error = E.error

      let used = E.used
      let flush = E.flush

      let eval raw state =
        match E.eval raw state with
        | `Flush state -> Lwt.return (`Flush state)
        | `Error error -> Lwt.return (`Error (state, error))
        | `End state -> Lwt.return (`End state)
    end

    include Helper.Encoder (E) (FS)
  end

  let write ~fs ~root:dotgit ~temp_dir ?(capacity = 0x100) ?(level = 4) ~ztmp
      ~raw value =
    let hash = digest value in
    let first, rest = explode hash in
    let encoder = E.default (capacity, value, level, ztmp) in
    let path = Fpath.(dotgit / "objects" / first / rest) in
    Log.debug (fun l -> l "Writing a new loose object %a." Fpath.pp path) ;
    FS.Dir.create fs Fpath.(dotgit / "objects" / first)
    >>= function
    | Error err -> Lwt.return Error.(v @@ FS.err_create path err)
    | Ok (true | false) -> (
        EInflated.to_file fs ~temp_dir path raw encoder
        >|= function
        | Error #fs_error as err -> err
        | Error (`Encoder err) -> Error.(v @@ Def.with_path path err)
        | Ok r ->
            Log.debug (fun l -> l "Wrote the object %s/%s" first rest) ;
            Ok (hash, r) )
end
