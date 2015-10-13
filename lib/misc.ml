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

module Log_make(S: sig val section: string end) =
struct
  include Log.Make(S)
      
  let int_of_level = function
    | Log.FATAL -> 4
    | Log.ERROR -> 3
    | Log.WARN  -> 2
    | Log.INFO  -> 1
    | Log.DEBUG -> 0

  let logk level fmt k =
    if int_of_level level >= int_of_level (Log.get_log_level ())
    then k (log level fmt)
  let fatalk fmt k = logk Log.FATAL fmt k
  let errork fmt k = logk Log.ERROR fmt k
  let warnk  fmt k = logk Log.WARN  fmt k
  let infok  fmt k = logk Log.INFO  fmt k
  let debugk fmt k = logk Log.DEBUG fmt k
end

module Log = Log.Make(struct let section = "misc" end)

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
  val pretty: t -> string
end

module type Set = sig
  include Set.S
  val pretty: t -> string
  val to_list: t -> elt list
  val of_list: elt list -> t
end

module type Map = sig
  include Map.S
  val pretty: ('a -> string) -> 'a t -> string
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

  let pretty s = match List.rev (elements s) with
    | []   -> "{}"
    | [x]  -> Printf.sprintf "{ %s }" (X.pretty x)
    | h::t -> Printf.sprintf "{ %s and %s }"
                (String.concat ", " (List.rev_map X.pretty t))
                (X.pretty h)
end

module Map (X: OrderedType) = struct

  include Map.Make(X)

  let keys m =
    List.map fst (bindings m)

  let of_alist l =
    List.fold_left (fun map (k, v)  -> add k v map) empty l

  let to_alist = bindings

  let pretty p m =
    let binding (k, v) = Printf.sprintf "(%s: %s)" (X.pretty k) (p v) in
    match List.rev (to_alist m) with
    | [] -> "{}"
    | x  -> Printf.sprintf "{ %s }" (String.concat " " (List.rev_map binding x))

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
  let pretty = string_of_int
end

module S = struct
  type t = string
  let compare = String.compare
  let pretty x = x
end

module IntMap = Map(I)

let string_forall f s =
  let rec aux i = i = String.length s || (f s.[i] && aux (i+1)) in
  aux 0

let string_exists f s =
  let rec aux i = i < String.length s && (f s.[i] || aux (i+1)) in
  aux 0

let string_mem c s =
  string_exists ((=) c) s

let string_chop_prefix t ~prefix =
  let lt = String.length t in
  let lp = String.length prefix in
  if lt < lp then None else
    let p = String.sub t 0 lp in
    if String.compare p prefix <> 0 then None
    else Some (String.sub t lp (lt - lp))

let string_chop_suffix t ~suffix =
  let lt = String.length t in
  let ls = String.length suffix in
  if lt < ls then None else
    let p = String.sub t (lt-ls) ls in
    if String.compare p suffix <> 0 then None
    else Some (String.sub t 0 (lt - ls))

let list_filter_map f l =
  List.fold_left (fun l elt ->
      match f elt with
      | None   -> l
      | Some x -> x :: l
    ) [] l
  |> List.rev

let pretty pp_hum t =
  let buf = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buf in
  pp_hum ppf t;
  Format.pp_print_flush ppf ();
  Buffer.contents buf
