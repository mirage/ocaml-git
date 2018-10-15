(* Copyright (c) 2018, Thomas Gazagnaire, Hannes Menhert *)

[@@@warning "-32"]

(* J'ai la flemme. *)

type t = string list

open Astring

let dir_sep_char = '/'
let dir_sep = String.of_char dir_sep_char
let dir_sep_sub = Astring.String.Sub.of_char '/'
let err_invalid_segment = Fmt.invalid_arg "%s is not a valid segment"
let empty = []
let root = []

let check_segment x =
  String.iter (function '/' -> err_invalid_segment x | _ -> ()) x ;
  x

let v s = List.filter (( <> ) "") @@ List.rev (String.cuts ~sep:dir_sep s)
let add t v = check_segment v :: t
let ( / ) = add
let append x y = y @ x
let ( // ) = append
let segs = List.rev
let basename = List.hd
let parent = List.tl
let compare a b = compare (List.rev a) (List.rev b)
let equal = ( = )
let pp ppf x = Fmt.pf ppf "%a" Fmt.(list ~sep:(unit "/") string) (List.rev x)
let to_string = Fmt.to_to_string pp
let of_segs l = List.rev (List.map check_segment l)
let ( + ) fpath gpath = List.fold_left Fpath.( / ) fpath (List.rev gpath)

(* ext *)

let ext_sep_char = '.'
let ext_sep = String.of_char ext_sep_char
let ext_sep_sub = String.Sub.of_char ext_sep_char
let eq_ext_sep x = x = ext_sep_char
let neq_ext_sep x = x <> ext_sep_char

let sub_multi_ext basename =
  let first_not_sep = String.Sub.drop ~sat:eq_ext_sep basename in
  String.Sub.drop ~sat:neq_ext_sep first_not_sep

let sub_single_ext basename =
  let name_dot, ext = String.Sub.span ~rev:true ~sat:neq_ext_sep basename in
  if String.Sub.exists neq_ext_sep name_dot then
    String.Sub.extend ~max:1 ~rev:true ext
  else String.Sub.empty

let sub_ext ?(multi = false) basename =
  if multi then sub_multi_ext basename else sub_single_ext basename

let rec sub_last_non_empty_seg = function
  | "" :: t -> sub_last_non_empty_seg t
  | basename :: _ -> String.Sub.of_string basename
  | [] -> String.Sub.empty

let sub_get_ext ?multi t = sub_ext ?multi (sub_last_non_empty_seg t)
let get_ext ?multi t = String.Sub.to_string (sub_get_ext ?multi t)

let has_ext e t =
  let ext = sub_get_ext ~multi:true t in
  if String.Sub.is_empty ext then false
  else if not String.(Sub.is_suffix ~affix:(sub e) ext) then false
  else if (not (String.is_empty e)) && e.[0] = ext_sep_char then true
  else
    let dot_index = String.Sub.length ext - String.length e - 1 in
    String.Sub.get ext dot_index = ext_sep_char

let ( @ ) t basename =
  let rec go acc = function
    | "" :: t -> go ("" :: acc) t
    | _basename :: t ->
        List.rev_append (String.Sub.to_string basename :: acc) t
    | [] -> []
  in
  go [] t

let split_ext ?multi t =
  let ext = sub_get_ext ?multi t in
  let basename = sub_last_non_empty_seg t in
  if String.Sub.is_empty ext then t @ basename, ext
  else
    let before_ext = String.Sub.start_pos ext - 1 in
    if String.Sub.stop_pos ext = String.Sub.length basename then
      t @ String.Sub.with_index_range basename ~last:before_ext, ext
    else
      let prefix = String.Sub.with_index_range basename ~last:before_ext in
      (t @ String.Sub.(base (concat [prefix; dir_sep_sub]))), ext

let rem_ext ?multi t = fst (split_ext ?multi t)
