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

let err_not_found n k =
  let str = Printf.sprintf "Git.Mem.%s: %s not found" n k in
  Lwt.fail (Invalid_argument str)

module type S =
sig
  include Minimal.S

  val read_inflated : t -> Hash.t -> ([ `Commit | `Tag | `Blob | `Tree ] * string) option Lwt.t
  val write_inflated : t -> kind:[ `Commit | `Tree | `Blob | `Tag ] -> Cstruct.t -> Hash.t Lwt.t
end

module Make
    (H : S.HASH with type Digest.buffer = Cstruct.t
                 and type hex = string)
    (P : Path.S)
    (L : Lock.S with type key = P.t)
    (I : S.INFLATE)
    (D : S.DEFLATE)
    (B : Value.BUFFER with type raw = string
                       and type fixe = Cstruct.t)
  : S with module Hash = H
       and module Path = P
       and module Lock = L
       and module Inflate = I
       and module Deflate = D
= struct
  module Hash = H
  module Path = P
  module Lock = L
  module Inflate = I
  module Deflate = D
  module Buffer = B

  module Value = Value.Raw(H)(I)(D)(B)
  module Reference = Reference.Make(H)(P)

  type t =
    { root         : Path.t
    ; dotgit       : Path.t
    ; compression  : int
    ; values       : (Hash.t, Value.t Lazy.t) Hashtbl.t
    ; inflated     : (Hash.t, ([ `Commit | `Tree | `Blob | `Tag ] * string)) Hashtbl.t
    ; refs         : (Reference.t, [ `H of Hash.t | `R of Reference.t ]) Hashtbl.t
    ; mutable head : Reference.head_contents option }

  type error = [ `Not_found ]

  let pp_error ppf = function
    | `Not_found -> Fmt.pf ppf "`Not_found"

  let root t = t.root
  let dotgit t = t.dotgit
  let compression t = t.compression

  let stores = Hashtbl.create 1024
  let default_root = Path.v "root"

  let reset t =
    Hashtbl.clear t.values;
    Hashtbl.clear t.inflated;
    Hashtbl.clear t.refs;
    t.head <- None

  let clear ?(root=default_root) () =
    let () = try reset (Hashtbl.find stores root) with Not_found -> () in
    Hashtbl.remove stores root

  let clear_all () =
    Hashtbl.iter (fun _ t -> reset t) stores

  let create ?(root = default_root) ?(dotgit = Path.(default_root / ".git")) ?(compression = 6) () =
    if compression < 0 || compression > 9
    then failwith "level should be between 0 and 9";

    let t =
      try Hashtbl.find stores root
      with Not_found ->
        let t =
          { root
          ; compression
          ; dotgit
          ; values   = Hashtbl.create 1024
          ; inflated = Hashtbl.create 1024
          ; refs     = Hashtbl.create 8
          ; head     = None }
        in

        Hashtbl.add stores root t;
        t
    in
    Lwt.return (Ok t : (t, error) result)

  let write t value =
    let hash = Value.digest value in
    let kind = match value with
      | Value.Commit _ -> `Commit
      | Value.Blob _ -> `Blob
      | Value.Tree _ -> `Tree
      | Value.Tag _ -> `Tag
    in

    if Hashtbl.mem t.values hash
    then Lwt.return (Ok (hash, 0))
    else match Value.to_raw value with
      | Error `Never -> assert false
      | Ok inflated ->
        Hashtbl.add t.values hash (lazy value);
        Hashtbl.add t.inflated hash (kind, inflated);
        Lwt.return (Ok (hash, String.length inflated) : ((Hash.t * int, error) result)) 

  let digest kind raw =
    let len = Cstruct.len raw in
    let ctx = Hash.Digest.init () in
    let hdr = Fmt.strf "%s %d\000%!"
        (match kind with
         | `Commit -> "commit"
         | `Blob -> "blob"
         | `Tree -> "tree"
         | `Tag -> "tag")
        len
    in
    Hash.Digest.feed ctx (Cstruct.of_string hdr);
    Hash.Digest.feed ctx raw;
    Hash.Digest.get ctx

  let write_inflated t ~kind inflated =
    let hash = digest kind inflated in

    if Hashtbl.mem t.values hash
    then Lwt.return hash
    else
      let value = lazy (match Value.of_raw ~kind inflated with Error (`Decoder err) -> raise (Failure err) | Ok value -> value) in
      Hashtbl.add t.inflated hash (kind, Cstruct.to_string inflated);
      Hashtbl.add t.values hash value;
      Lwt.return hash

  let read_inflated t h =
    try Lwt.return (Some (Hashtbl.find t.inflated h))
    with Not_found -> Lwt.return_none

  let read t h =
    try Lwt.return (Ok (Lazy.force (Hashtbl.find t.values h)))
    with Not_found -> Lwt.return (Error `Not_found)

  let keys t =
    Hashtbl.fold (fun k _ l -> k :: l) t []

  let list t =
    Lwt.return (keys t.values)

  let exists t h =
    Lwt.return (Hashtbl.mem t.values h)

  let size t h =
    read t h >|= function
    | Ok (Value.Blob v) -> Ok (Value.Blob.F.length v)
    | _ -> Error `Not_found

  let read_exn t h =
    read t h >>= function
    | Error _ -> err_not_found "read_exn" (Hash.to_hex h)
    | Ok v -> Lwt.return v

  let contents t =
    list t >>= fun hashes ->
    Lwt_list.map_s (fun h -> read_exn t h >|= fun value -> h, value) hashes >|= fun values ->
    (Ok values : ((Hash.t * Value.t) list, error) result)

  module Ref =
  struct
    type error = [ `Not_found ]

    let pp_error ppf = function
      | `Not_found -> Fmt.pf ppf "`Not_found"

    module Graph = Reference.Map

    let list t =
      let graph, rest =
        Hashtbl.fold
          (fun k -> function
             | `R ptr -> fun (a, r) -> a, (k, ptr) :: r
             | `H hash -> fun (a, r) -> Graph.add k hash a, r)
          t.refs (Graph.empty, [])
      in

      let graph =
        List.fold_left (fun a (k, ptr) ->
            try let v = Graph.find ptr a in
              Graph.add k v a
            with Not_found -> a)
          graph rest
      in

      Graph.fold (fun k v a -> (k, v) :: a) graph []
      |> Lwt.return

    let exists t ref =
      Lwt.return (Hashtbl.mem t.refs ref)

    let rec read t r =
      try match Hashtbl.find t.refs r with
        | `H s -> Lwt.return (Ok (r, Reference.Hash s))
        | `R r -> read t r
      with Not_found -> Lwt.return (Error `Not_found)

    let remove t ?locks:_ r =
      Hashtbl.remove t.refs r;
      Lwt.return (Ok ())

    let write t ?locks r value =
      let head_contents = match value with
        | Reference.Hash hash -> `H hash
        | Reference.Ref refname -> `R refname
      in

      let lock = match locks with
        | Some locks -> Some (Lock.make locks (Reference.to_path r))
        | None -> None
      in

      Lock.with_lock lock
      @@ (fun () ->
          Hashtbl.replace t.refs r head_contents; Lwt.return (Ok ()))

    let test_and_set t ?locks:_ r ~test ~set =
      (* XXX(dinosaure): not sure about the semantic. *)
      let v =
        try Some (Hashtbl.find t.refs r)
            |> function Some (`R refname) -> Some (Reference.Ref refname)
                      | Some (`H hash) -> Some (Reference.Hash hash) 
                      | None -> None
        with Not_found -> None in
      let replace () = match set with
        | None   -> Hashtbl.remove t.refs r
        | Some (Reference.Hash hash) -> Hashtbl.replace t.refs r (`H hash)
        | Some (Reference.Ref refname) -> Hashtbl.replace t.refs r (`R refname)
      in
      match v, test with
      | None  , None -> replace (); Lwt.return (Ok true)
      | Some (Reference.Hash x), Some (Reference.Hash y) when Hash.equal x y -> replace (); Lwt.return (Ok true)
      | _ -> Lwt.return (Ok false)
  end
end
