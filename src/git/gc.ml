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

module Make (S : Minimal.S with type Hash.Digest.buffer = Cstruct.t
                            and type Hash.hex = string)
= struct
  module Store = S
  module PACKEncoder = Pack.MakePACKEncoder(Store.Hash)(Store.Deflate)

  let delta ?(window = 10) ?(depth = 50) git objects =
    let open Lwt_result in

    let names = Hashtbl.create 1024 in

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
        PACKEncoder.Entry.make
          hash
          ?name
          kind
          (Store.Value.F.length value)
      in

      Lwt.return entry
    in

    let ( >?= ) a f = Lwt_result.map_err f a in

    Lwt.Infix.(Lwt_list.iter_p (fun (_, value) -> match value with
        | Store.Value.Tree tree ->
          Lwt_list.iter_p
            (fun entry ->
               Hashtbl.add names
                 entry.Store.Value.Tree.node
                 entry.Store.Value.Tree.name;
               Lwt.return ())
            (tree :> Store.Value.Tree.entry list)
        | _ -> Lwt.return ())
        objects >|= fun () -> Ok ())
    >>= fun () ->
    Lwt.Infix.(Lwt_list.map_p make objects >|= fun entries -> Ok entries)
    >>= fun entries ->
    (PACKEncoder.Delta.deltas
       entries
       Lwt.Infix.(fun hash -> Store.read_inflated git hash >|= function
         | Some (_, raw) -> Some raw
         | None -> None)
       (fun _ -> false)
       window depth
     >?= fun err -> `PackEncoder err)

  let delta_all ?window ?depth git =
    let open Lwt.Infix in

    Store.contents git >>= function
    | Ok objects -> delta ?window ?depth git objects
    | Error err -> Lwt.return (Error (`Store err))

  let make ?window ?depth git objects =
    let open Lwt.Infix in

    let ztmp = Cstruct.create 0x8000 in

    delta ?window ?depth git objects >>= function
    | Ok entries ->
      let state = PACKEncoder.default ztmp entries in

      Lwt.return (Ok state)
    | Error _ as err -> Lwt.return err

  let make_all ?window ?depth git =
    let open Lwt.Infix in

    let ztmp = Cstruct.create 0x8000 in

    delta_all ?window ?depth git >>= function
    | Ok entries ->
      let state = PACKEncoder.default ztmp entries in

      Lwt.return (Ok state)
    | Error _ as err -> Lwt.return err
end
