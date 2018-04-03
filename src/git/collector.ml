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

module type STORE = sig

  module Hash: S.HASH
  module Value: Value.S with module Hash = Hash
  module Deflate: S.DEFLATE
  module PEnc: Pack.P
    with module Hash = Hash
     and module Deflate = Deflate

  type t
  type error

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  val pp_error: error Fmt.t
  val read_inflated: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  val contents: t -> ((Hash.t * Value.t) list, error) result Lwt.t
end

module Make (S: STORE) = struct

  module Store = S

  module Log =
  struct
    let src = Logs.Src.create "git.gc" ~doc:"logs git's GC event"
    include (val Logs.src_log src : Logs.LOG)
  end

  let cstruct_copy cs =
    let ln = Cstruct.len cs in
    let rs = Cstruct.create ln in
    Cstruct.blit cs 0 rs 0 ln;
    rs

  let delta ?(window = `Object 10) ?(depth = 50) git objects =
    let names = Hashtbl.create 1024 in
    let memory, window = match window with `Memory v -> true, v | `Object v -> false, v in

    let make (hash, value) =
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
        S.PEnc.Entry.make
          hash
          ?name
          kind
          (Store.Value.F.length value)
      in

      Log.debug (fun l -> l ~header:"delta" "Add the object %a in the new PACK file."
                    S.Hash.pp hash);

      entry in

    let canonicalize entries =
      List.fold_left
        (fun acc o ->
           let hash = Store.Value.digest o in
           Store.Hash.Map.add hash o acc)
        Store.Hash.Map.empty entries
      |> Store.Hash.Map.bindings in

    let ( >?= ) a f = Lwt_result.map_err f a in

    canonicalize objects
    |> fun objects ->
    List.iter
      (function
        | _, Store.Value.Tree tree ->
          Store.Value.Tree.iter
            (fun entry ->
               Hashtbl.add names
                 entry.Store.Value.Tree.node
                 entry.Store.Value.Tree.name)
            tree
        | _ -> ())
      objects
    |> fun () ->
    List.map make objects
    |> fun entries ->
    S.PEnc.Delta.deltas
      ~memory
      entries
      Lwt.Infix.(fun hash ->
          Log.debug (fun l -> l ~header:"delta" "Ask to try to delta-ify the object %a." S.Hash.pp hash);

          Store.read_inflated git hash >|= function
          | Some (_, raw) -> Some (cstruct_copy raw)
          | None -> None)
      (fun _ -> false)
      window depth
    >?= (fun err -> `Delta err)

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
      let state = S.PEnc.default ztmp entries in

      Lwt.return (Ok state)
    | Error _ as err -> Lwt.return err

  let make_all ?window ?depth git =
    let open Lwt.Infix in

    let ztmp = Cstruct.create 0x8000 in

    delta_all ?window ?depth git >>= function
    | Ok entries ->
      let state = S.PEnc.default ztmp entries in

      Lwt.return (Ok state)
    | Error _ as err -> Lwt.return err

  exception PackEncoder of S.PEnc.error

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
        match S.PEnc.eval (value ~default:empty !src) dtmp !state with
        | `Flush state' ->
          write := !write + (S.PEnc.used_out state');
          state := S.PEnc.flush 0 (Cstruct.len dtmp) state';
          Lwt.return (Some (Cstruct.sub dtmp 0 (S.PEnc.used_out state')))
        | `End (state', _) ->
          if S.PEnc.used_out state' > 0
          then begin
            state := S.PEnc.flush 0 0 state';
            Lwt.return (Some (Cstruct.sub dtmp 0 (S.PEnc.used_out state')))
          end else begin
            state := state';
            let graph = S.PEnc.Map.fold (fun key value acc -> Graph.add key value acc) (S.PEnc.idx state') Graph.empty in
            (* XXX(dinosaure): can be optimized because structurally the same. *)
            Lwt_mvar.put mvar graph >>= fun () -> Lwt.return None
          end
        | `Error (state', err) ->
          state := state';
          Lwt.fail (PackEncoder err)
        | `Await state' -> match !src with
          | Some _ ->
            src := None;
            state := S.PEnc.finish state';
            stream ()
          | None ->
            let expect = S.PEnc.expect state' in
            Store.read_inflated git expect >>= (function
                | Some (_, raw) ->
                  src := Some raw;
                  Lwt.return raw
                | None -> assert false) >>= fun raw ->
            state := S.PEnc.refill 0 (Cstruct.len raw) state';
            stream ()
      in

      Lwt.return (Ok (stream, mvar))
end
