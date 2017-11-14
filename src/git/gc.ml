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

module type STORE =
sig
  module Hash
    : S.HASH
  module Path
    : S.PATH
  module Value
    : Value.S
      with module Hash = Hash
  module Deflate
    : S.DEFLATE
  module PACKEncoder
    : Pack.ENCODER
        with module Hash = Hash
         and module Deflate = Deflate

  type t
  type error

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  val pp_error : error Fmt.t
  val read_inflated : t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  val contents : t -> ((Hash.t * Value.t) list, error) result Lwt.t
end

module Make (S : STORE with type Hash.Digest.buffer = Cstruct.t
                        and type Hash.hex = string)
= struct
  module Store = S

  let delta ?(window = `Object 10) ?(depth = 50) git objects =
    let open Lwt_result in

    let names = Hashtbl.create 1024 in
    let memory, window = match window with `Memory v -> true, v | `Object v -> false, v in

    let make value =
      let hash = Store.Value.digest value in

      let name =
        try Some (Hashtbl.find names hash)
        with Not_found -> None
      in

      let kind = match value with
        | Store.Value.Commit _ -> Pack.Kind.Commit
        | Store.Value.Tree _ -> Pack.Kind.Tree
        | Store.Value.Tag _ -> Pack.Kind.Tag
        | Store.Value.Blob _ -> Pack.Kind.Blob
      in

      let entry =
        S.PACKEncoder.Entry.make
          hash
          ?name
          kind
          (Store.Value.F.length value)
      in

      Lwt.return entry
    in

    let ( >?= ) a f = Lwt_result.map_err f a in

    Lwt.Infix.(Lwt_list.iter_p (function
        | Store.Value.Tree tree ->
          Lwt_list.iter_p
            (fun entry ->
               Hashtbl.add names
                 entry.Store.Value.Tree.node
                 entry.Store.Value.Tree.name;
               Lwt.return ())
            (Store.Value.Tree.to_list tree)
        | _ -> Lwt.return ())
        objects >|= fun () -> Ok ())
    >>= fun () ->
    Lwt.Infix.(Lwt_list.map_p make objects >|= fun entries -> Ok entries)
    >>= fun entries ->
    (S.PACKEncoder.Delta.deltas
       ~memory
       entries
       Lwt.Infix.(fun hash ->
           Store.read_inflated git hash >|= function
           | Some (_, raw) -> Some raw
           | None -> None)
       (fun _ -> false)
       window depth
     >?= fun err -> `Delta err)

  let delta_all ?window ?depth git =
    let open Lwt.Infix in

    let snd (_, x) = Lwt.return x in

    Lwt_result.(Store.contents git >>= fun x -> ok (Lwt_list.map_p snd x)) >>= function
    | Ok objects -> delta ?window ?depth git objects
    | Error err -> Lwt.return (Error (`Store err))

  let make ?window ?depth git objects =
    let open Lwt.Infix in

    let ztmp = Cstruct.create 0x8000 in

    delta ?window ?depth git objects >>= function
    | Ok entries ->
      let state = S.PACKEncoder.default ztmp entries in

      Lwt.return (Ok state)
    | Error _ as err -> Lwt.return err

  let make_all ?window ?depth git =
    let open Lwt.Infix in

    let ztmp = Cstruct.create 0x8000 in

    delta_all ?window ?depth git >>= function
    | Ok entries ->
      let state = S.PACKEncoder.default ztmp entries in

      Lwt.return (Ok state)
    | Error _ as err -> Lwt.return err

  exception PackEncoder of S.PACKEncoder.error

  module Graph = Map.Make(S.Hash)

  let make_stream git ?(window = `Object 10) ?(depth = 50) objects =
    let open Lwt.Infix in

    make git ~window ~depth objects >>= function
    | Error _ as err -> Lwt.return err
    | Ok state ->
      let dtmp = Cstruct.create 0x8000 in
      let empty = Cstruct.create 0 in

      let value ~default = function Some x -> x | None -> default in
      let mvar = Lwt_mvar.create_empty () in

      let src   = ref None in
      let state = ref state in
      let write = ref 0 in

      let rec stream () =
        match S.PACKEncoder.eval (value ~default:empty !src) dtmp !state with
        | `Flush state' ->
          write := !write + (S.PACKEncoder.used_out state');
          state := S.PACKEncoder.flush 0 (Cstruct.len dtmp) state';
          Lwt.return (Some (Cstruct.sub dtmp 0 (S.PACKEncoder.used_out state')))
        | `End (state', _) ->
          if S.PACKEncoder.used_out state' > 0
          then begin
            state := S.PACKEncoder.flush 0 0 state';
            Lwt.return (Some (Cstruct.sub dtmp 0 (S.PACKEncoder.used_out state')))
          end else begin
            state := state';
            let graph = S.PACKEncoder.Radix.fold (fun (key, value) acc -> Graph.add key value acc) Graph.empty (S.PACKEncoder.idx state') in
            Lwt_mvar.put mvar graph >>= fun () -> Lwt.return None
          end
        | `Error (state', err) ->
          state := state';
          Lwt.fail (PackEncoder err)
        | `Await state' -> match !src with
          | Some _ ->
            src := None;
            state := S.PACKEncoder.finish state';
            stream ()
          | None ->
            let expect = S.PACKEncoder.expect state' in
            Store.read_inflated git expect >>= (function
                | Some (_, raw) ->
                  src := Some raw;
                  Lwt.return raw
                | None -> assert false) >>= fun raw ->
            state := S.PACKEncoder.refill 0 (Cstruct.len raw) state';
            stream ()
      in

      Lwt.return (Ok (stream, mvar))
end
