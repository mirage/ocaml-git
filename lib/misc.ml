(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

module Log_make (S: sig val section: string end) = struct
  let src =
    let doc = Printf.sprintf "logs git's %s events" S.section in
    Logs.Src.create ("git." ^ S.section) ~doc
  module Log = (val Logs.src_log src : Logs.LOG)
  include Log
end

let sp  = '\x20'
let nul = '\x00'
let lf  = '\x0a'
let lt  = '<'
let gt  = '>'

let sp_str = String.make 1 sp
let nul_str = String.make 1 nul

let input_key_value buf ~key:expected input_value =
  let error actual =
    Mstruct.parse_error_buf buf "keys: [actual: %s] [expected: %s]" actual expected in
  let key =
    match Mstruct.get_string_delim buf sp with
    | None   -> error "<none>"
    | Some k -> k in
  if key <> expected then
    error key
  else
    match Mstruct.get_delim buf lf input_value with
    | None   -> Mstruct.parse_error_buf buf "no value to input"
    | Some v -> v

let str_buffer = Bytes.create 4
let add_be_uint32 buf i =
  EndianString.BigEndian.set_int32 str_buffer 0 i;
  Buffer.add_string buf str_buffer

let with_buffer fn =
  let buf = Buffer.create 1024 in
  fn buf;
  Buffer.contents buf

let with_buffer' fn =
  Cstruct.of_string (with_buffer fn)

module OP = struct

  let (/) = Filename.concat

end

let inverse_assoc l =
  List.rev_map (fun (k, v) -> (v, k)) l

let try_assoc elt l =
  try Some (List.assoc elt l)
  with Not_found -> None

module type OrderedType = sig
  include Set.OrderedType
  val pp: t Fmt.t
end

module type Set = sig
  include Set.S
  val pp: t Fmt.t
  val to_list: t -> elt list
  val of_list: elt list -> t
end

module type Map = sig
  include Map.S
  val pp: 'a Fmt.t -> 'a t Fmt.t
  val keys: 'a t -> key list
  val to_alist: 'a t -> (key * 'a) list
  val of_alist: (key * 'a) list -> 'a t
  val add_multi: key -> 'a -> 'a list t -> 'a list t
end


module Set (X: OrderedType) = struct

  include Set.Make(X)

  let of_list l =
    List.fold_left (fun set elt -> add elt set) empty l

  let to_list = elements

  let pp ppf s = match List.rev (elements s) with
    | []   -> Fmt.string ppf "{}"
    | [x]  -> Fmt.pf ppf "{ %a }" X.pp x
    | h::t -> Fmt.(pf ppf "{ %a and %a }"
                     (list ~sep:(const string ", ") X.pp) (List.rev t)
                     X.pp h)
end

module Map (X: OrderedType) = struct

  include Map.Make(X)

  let keys m =
    List.map fst (bindings m)

  let of_alist l =
    List.fold_left (fun map (k, v)  -> add k v map) empty l

  let to_alist = bindings

  let pp p ppf m =
    let binding ppf (k, v) = Fmt.pf ppf "(%a: %a)" X.pp k p v in
    match List.rev (to_alist m) with
    | [] -> Fmt.string ppf "{}"
    | x  -> Fmt.(pf ppf "{ %a }"
                   (list ~sep:(const string " ") binding) (List.rev x))

  let add_multi key data t =
    try
      let l = find key t in
      add key (data :: l) t
    with Not_found ->
      add key [data] t

end

module I = struct
  type t = int
  let compare = compare
  let pp = Fmt.int
end

module S = struct
  type t = string
  let compare = String.compare
  let pp = Fmt.string
end

module IntMap = Map(I)

let list_filter_map f l =
  List.fold_left (fun l elt ->
      match f elt with
      | None   -> l
      | Some x -> x :: l
    ) [] l
  |> List.rev
