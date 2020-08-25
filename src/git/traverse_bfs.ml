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

let src = Logs.Src.create "git.traverse" ~doc:"logs git's traverse event"

module Log = (val Logs.src_log src : Logs.LOG)

module type STORE = sig
  module Hash : S.HASH

  module Value : Value.S with type hash = Hash.t

  type t

  val root : t -> Fpath.t

module Make (S : STORE) = struct
  open S
  val read_exn : t -> Hash.t -> Value.t Lwt.t
end

  (* XXX(dinosaure): convenience and common part between the file-system and
     the mem back-end - to avoid redundant code. *)

  let fold t
      (f :
        'acc -> ?name:Path.t -> length:int64 -> Hash.t -> Value.t -> 'acc Lwt.t)
      ~path acc hash =
    let names = Hashtbl.create 0x100 in
    let open Lwt.Infix in
    let rec walk close rest queue acc =
      match rest with
      | [] -> (
        match Queue.pop queue with
        | rest -> walk close [rest] queue acc
        | exception Queue.Empty -> Lwt.return acc )
      | hash :: rest -> (
          if Hash.Set.mem hash close then walk close rest queue acc
          else
            let close' = Hash.Set.add hash close in
            read t hash
            >>= function
            | Ok (Value.Commit commit as value) ->
                let rest' = Value.Commit.tree commit :: rest in
                List.iter
                  (fun x -> Queue.add x queue)
                  (Value.Commit.parents commit) ;
                f acc ~length:(Value.Commit.length commit) hash value
                >>= fun acc' -> walk close' rest' queue acc'
            | Ok (Value.Tree tree as value) ->
                let path =
                  try Hashtbl.find names hash with Not_found -> path
                in
                Lwt_list.iter_s
                  (fun {Value.Tree.name; node; _} ->
                    Hashtbl.add names node Path.(path / name) ;
                    Lwt.return () )
                  (Value.Tree.to_list tree)
                >>= fun () ->
                let rest' =
                  rest
                  @ List.map
                      (fun {Value.Tree.node; _} -> node)
                      (Value.Tree.to_list tree)
                in
                f acc ~name:path ~length:(Value.Tree.length tree) hash value
                >>= fun acc' -> walk close' rest' queue acc'
            | Ok (Value.Blob blob as value) ->
                let path =
                  try Hashtbl.find names hash with Not_found -> path
                in
                f acc ~name:path ~length:(Value.Blob.length blob) hash value
                >>= fun acc' -> walk close' rest queue acc'
            | Ok (Value.Tag tag as value) ->
                Queue.add (Value.Tag.obj tag) queue ;
                f acc ~length:(Value.Tag.length tag) hash value
                >>= fun acc' -> walk close' rest queue acc'
            | Error err ->
                Log.err (fun l ->
                    l ~header:"fold"
                      "Retrieve an error when we try to read the Git object \
                       %a: %a."
                      Hash.pp hash pp_error err ) ;
                walk close' rest queue acc )
    in
    walk Hash.Set.empty [hash] (Queue.create ()) acc

  let iter t f hash =
    fold t
      (fun () ?name:_ ~length:_ hash value -> f hash value)
      ~path:Path.empty () hash
end
