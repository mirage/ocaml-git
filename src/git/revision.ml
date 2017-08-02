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

module Make
    (Store : Store.S with type Hash.Digest.buffer = Cstruct.t
                      and type FileSystem.File.error = [ `System of string ]
                      and type FileSystem.File.raw = Cstruct.t
                      and type FileSystem.Dir.error = [ `System of string ]
                      and type FileSystem.Mapper.error = [ `System of string ]
                      and type FileSystem.Mapper.raw = Cstruct.t)
= struct
  type t =
    | Reference of Store.Reference.t (* <refname> *)
    | Commit of id                   (* <id> *)
    | Parent of t * int              (* <rev>^<n> *)
    | Ancestor of t * int            (* <rev>~<n> *)
  and id =
    | Incomplete of string
    | Complete of Store.Hash.t

  let head = Reference Store.Reference.head

  let parent rev n   = Parent (rev, n)
  let ancestor rev n = Ancestor (rev, n)

  let from_hash hash = Commit (Complete hash)

  let rec commit git hash =
    let open Lwt.Infix in

    Store.read git hash >>= function
    | Ok (Store.Value.Commit commit) -> Lwt.return (Ok commit)
    | Ok (Store.Value.Tag tag) -> commit git (Store.Value.Tag.obj tag)
    | Ok _ -> Lwt.return (Error (`Expected_commit hash))
    | Error #Store.error as err -> Lwt.return err

  let rec normalize git rev =
    let open Lwt.Infix in

    match rev with
    | Commit (Complete hash) -> Lwt.return (Ok hash)
    | Commit (Incomplete partial) -> assert false
    | Parent (rev, n) ->
      normalize git rev >>= (function
          | Ok hash ->
            (commit git hash >>= function
              | Ok commit ->
                Lwt.try_bind
                  (fun () -> List.nth (Store.Value.Commit.parents commit) n |> Lwt.return)
                  (fun hash -> Lwt.return (Ok hash))
                  (fun _ -> Lwt.return (Error (`Invalid_parent (hash, n))))
              | Error _ as err -> Lwt.return err)
          | Error _ as err -> Lwt.return err)
    | Ancestor (rev, n) ->
      normalize git rev >>= (function
          | Ok hash ->
            let rec go hash = function
              | 0 -> Lwt.return (Ok hash)
              | n -> commit git hash >>= function
                | Error _ as err -> Lwt.return err
                | Ok commit ->
                  Lwt.try_bind
                    (fun () -> Lwt.return (List.hd (Store.Value.Commit.parents commit)))
                    (fun hash -> go hash (n - 1))
                    (fun _ -> Lwt.return (Error (`Invalid_parent (hash, n))))
            in
            go hash n
          | Error _ as err -> Lwt.return err)
    | Reference reference ->
      Store.Ref.graph git >>= function
      | Error #Store.Ref.error as err -> Lwt.return err
      | Ok graph ->
        Store.Ref.normalize graph (Store.Reference.Ref reference) >>= function
        | Error #Store.Ref.error as err -> Lwt.return err
        | Ok _ as value-> Lwt.return value

  module Range =
  struct
    type nonrec t =
      | Include     of t (* <rev> *)
      | Exclude     of t (* ^<rev> *)
      | Diff        of t * t (* <rev1>..<rev2> différence
                                semantically, it's the same than [^<rev1> <rev2>] *)
      | Delta       of t * t (* <rev1>...<rev2> différence symétrique *)
      | Parents     of t (* <rev>^@ *)
      | PParents    of t (* <rev>^! *)

    module E = Store.Hash.Set

    let to_visit git hash =
      let open Lwt.Infix in

      Store.read git hash >|= function
      | Ok (Store.Value.Commit commit) -> Store.Value.Commit.parents commit
      | Ok (Store.Value.Tree tree) -> []
      | Ok (Store.Value.Blob _) -> []
      | Ok (Store.Value.Tag tag) -> [ Store.Value.Tag.obj tag ]
      | Error _ -> []
        (* XXX(dinosaure): we silent the error. *)

    let rec walk git close rest to_visit fadd fcompute acc =
      let open Lwt.Infix in

      match rest with
      | [] -> Lwt.return acc
      | hash :: rest ->
        if E.exists ((=) hash) close
        then walk git close rest to_visit fadd fcompute acc
        else begin
          fcompute hash acc >>= fun acc' ->
          to_visit git hash >|= fadd rest >>= fun rest' ->
          let close' = E.add hash close in

          walk git close' rest' to_visit fadd fcompute acc'
        end

    let normalize git range =
      let open Lwt.Infix in

      let fadd = fun a b -> a @ b in
      let fcompute = fun hash e -> Lwt.return (E.add hash e) in

      match range with
      | Include rev ->
        (normalize git rev >>= function
          | Ok hash -> walk git E.empty [ hash ] to_visit fadd fcompute E.empty
          | Error _ -> Lwt.return E.empty)
      | Exclude rev ->
        (normalize git rev >>= function
          | Error _ -> Lwt.return E.empty
          | Ok hash -> commit git hash >>= function
            | Error _ -> Lwt.return E.empty
            | Ok commit ->
              let exclude = E.of_list (Store.Value.Commit.parents commit) in
              walk git exclude [ hash ] to_visit fadd fcompute E.empty)
      | Diff (rev1, rev2) ->
        (normalize git rev1 >>= function
          | Error _ -> Lwt.return E.empty
          | Ok hash -> walk git E.empty [ hash ] to_visit fadd fcompute E.empty)
        >>= fun set1 ->
        normalize git rev2 >>= (function
            | Error _ -> Lwt.return E.empty
            | Ok hash -> walk git E.empty [ hash ] to_visit fadd fcompute E.empty)
        >|= fun set2 -> E.diff set2 set1
      | Delta (rev1, rev2) ->
        (normalize git rev1 >>= function
          | Error _ -> Lwt.return E.empty
          | Ok hash -> walk git E.empty [ hash ] to_visit fadd fcompute E.empty)
        >>= fun set ->
        (normalize git rev2 >>= function
          | Error _ -> Lwt.return E.empty
          | Ok hash -> walk git set [ hash ] to_visit fadd fcompute E.empty)
        >|= E.inter set
      | Parents rev ->
        (normalize git rev >>= function
          | Error _ -> Lwt.return E.empty
          | Ok hash -> commit git hash >>= function
            | Error _ -> Lwt.return E.empty
            | Ok commit -> walk git (E.singleton hash) (Store.Value.Commit.parents commit) to_visit fadd fcompute E.empty)
      | PParents rev ->
        (normalize git rev >>= function
          | Error _ -> Lwt.return E.empty
          | Ok hash -> commit git hash >>= function
            | Error _ -> Lwt.return E.empty
            | Ok commit ->
              let exclude = E.of_list (Store.Value.Commit.parents commit) in
              walk git exclude [ hash ] to_visit fadd fcompute E.empty)
  end
end
