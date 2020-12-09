(* (c) 2015 Daniel C. Bünzli
 * (c) 2020 Romain Calascibetta
 *
 * This implementation differs a bit from [fpath]
 * where absolute path is not valid and we manipulate
 * only POSIX path - the backend takes care about Windows
 * and POSIX paths then. *)

type t = string (* non empty *)

let dir_sep = "/"
let dir_sep_char = '/'
let error_msgf fmt = Fmt.kstr (fun err -> Error (`Msg err)) fmt

let validate_and_collapse_seps p =
  let max_idx = String.length p - 1 in
  let rec with_buf b last_sep k i =
    if i > max_idx then Ok (Bytes.sub_string b 0 k)
    else
      let c = p.[i] in
      if c = '\x00' then error_msgf "Malformed reference: %S" p
      else if c <> dir_sep_char then (
        Bytes.set b k c;
        with_buf b false (k + 1) (i + 1) )
      else if not last_sep then (
        Bytes.set b k c;
        with_buf b true (k + 1) (i + 1) )
      else with_buf b true k (i + 1)
  in
  let rec try_no_alloc last_sep i =
    if i > max_idx then Ok p
    else
      let c = p.[i] in
      if c = '\x00' then error_msgf "Malformed reference: %S" p
      else if c <> dir_sep_char then try_no_alloc false (i + 1)
      else if not last_sep then try_no_alloc true (i + 1)
      else
        let b = Bytes.of_string p in
        with_buf b true i (i + 1)
  in
  let start =
    if max_idx > 0 then if p.[0] = dir_sep_char then 1 else 0 else 0
  in
  try_no_alloc false start

let of_string p =
  if p = "" then error_msgf "Empty path"
  else
    match validate_and_collapse_seps p with
    | Ok p ->
        if p.[0] = dir_sep_char then error_msgf "Absolute reference" else Ok p
    | Error _ as err -> err

let v p =
  match of_string p with Ok v -> v | Error (`Msg err) -> invalid_arg err

let is_seg s =
  let zero = String.contains s '\x00' in
  let sep = String.contains s dir_sep_char in
  (not zero) && not sep

let add_seg p seg =
  if not (is_seg seg) then Fmt.invalid_arg "Invalid segment: %S" seg;
  let sep = if p.[String.length p - 1] = dir_sep_char then "" else dir_sep in
  String.concat sep [ p; sep ]

let append p0 p1 =
  if p1.[0] = dir_sep_char then p1
  else
    let sep =
      if p0.[String.length p0 - 1] = dir_sep_char then "" else dir_sep
    in
    String.concat sep [ p0; p1 ]

let ( / ) p seg = add_seg p seg
let ( // ) p0 p1 = append p0 p1
let segs p = Astring.String.cuts ~sep:dir_sep p
let pp ppf p = Fmt.string ppf p
let to_string x = x
let equal p0 p1 = String.equal p0 p1
let compare p0 p1 = String.compare p0 p1
let head = "HEAD"
let master = "refs/heads/master"

module Ordered = struct
  type nonrec t = t

  let compare a b = compare a b
end

module Map = Map.Make (Ordered)
module Set = Set.Make (Ordered)

type 'uid contents = Uid of 'uid | Ref of t

let equal_contents ~equal:equal_uid a b =
  match a, b with
  | Uid a, Uid b -> equal_uid a b
  | Ref a, Ref b -> equal a b
  | _ -> false

let pp_contents ~pp:pp_uid ppf = function
  | Ref v -> pp ppf v
  | Uid v -> pp_uid ppf v

let compare_contents ~compare:compare_uid a b =
  let inf = -1 and sup = 1 in
  match a, b with
  | Ref a, Ref b -> compare a b
  | Uid a, Uid b -> compare_uid a b
  | Ref _, _ -> sup
  | Uid _, _ -> inf

open Carton

module Packed = struct
  type 'uid elt = Ref of t * 'uid | Peeled of 'uid
  type 'uid packed = 'uid elt list
  type ('fd, 's) input_line = 'fd -> (string option, 's) io

  (* XXX(dinosaure): [Digestif.of_hex] is able to ignore '\n' character.
   * This code relies on this behavior. *)

  let load { Carton.bind; Carton.return } ~input_line ~of_hex fd =
    let ( >>= ) = bind in
    let rec go acc =
      input_line fd >>= function
      | Some line -> (
          match Astring.String.head line with
          | None -> go acc
          | Some '#' -> go acc
          | Some '^' ->
              let uid = String.sub line 1 (String.length line - 1) in
              let uid = of_hex uid in
              go (Peeled uid :: acc)
          | Some _ -> (
              match Astring.String.cut ~sep:" " line with
              | Some (uid, reference) ->
                  let reference = v reference in
                  let uid = of_hex uid in
                  go (Ref (reference, uid) :: acc)
              | None -> go acc ) )
      | None -> return (List.rev acc)
    in
    go []

  exception Found

  let exists reference packed =
    let res = ref false in
    let f = function
      | Ref (reference', _) ->
          if equal reference reference' then (
            res := true;
            raise Found )
      | _ -> ()
    in
    (try List.iter f packed with Found -> ());
    !res

  let get reference packed =
    let res = ref None in
    let f = function
      | Ref (reference', uid) ->
          if equal reference reference' then (
            res := Some uid;
            raise Found )
      | _ -> ()
    in
    (try List.iter f packed with Found -> ());
    !res

  let remove reference packed =
    let fold acc = function
      | Ref (reference', uid) ->
          if equal reference reference' then acc
          else Ref (reference', uid) :: acc
      | v -> v :: acc
    in
    List.rev (List.fold_left fold [] packed)
end

type ('t, 'uid, 'error, 's) store = {
  atomic_wr : 't -> t -> string -> ((unit, 'error) result, 's) io;
  atomic_rd : 't -> t -> ((string, 'error) result, 's) io;
  uid_of_hex : string -> 'uid option;
  uid_to_hex : 'uid -> string;
  packed : 'uid Packed.packed;
}

let reword_error f = function Ok v -> Ok v | Error err -> Error (f err)

let contents store str =
  match store.uid_of_hex (String.trim str) with
  | Some uid -> Uid uid
  | None -> (
      let is_sep chr = Astring.Char.Ascii.is_white chr || chr = ':' in
      match Astring.String.fields ~empty:false ~is_sep str with
      | [ _ref; value ] -> Ref (v value)
      | _ -> Fmt.invalid_arg "Invalid reference contents: %S" str )

let resolve { Carton.bind; Carton.return } t store reference =
  let ( >>= ) = bind in

  let rec go visited reference =
    store.atomic_rd t reference >>= function
    | Error _ -> (
        match Packed.get reference store.packed with
        | Some uid -> return (Ok uid)
        | None -> return (Error (`Not_found reference)) )
    | Ok str -> (
        match contents store str with
        | Uid uid -> return (Ok uid)
        | Ref reference ->
            if List.exists (equal reference) visited then return (Error `Cycle)
            else go (reference :: visited) reference )
  in
  go [ reference ] reference

let read { Carton.bind; Carton.return } t store reference =
  let ( >>= ) = bind in

  store.atomic_rd t reference >>= function
  | Error _ -> (
      match Packed.get reference store.packed with
      | Some uid -> return (Ok (Uid uid))
      | None -> return (Error (`Not_found reference)) )
  | Ok str -> return (Ok (contents store str))

let write { Carton.bind; Carton.return } t store reference contents =
  let ( >>= ) = bind in
  let ( >>| ) x f = x >>= fun x -> return (f x) in

  let str =
    match contents with
    | Uid uid -> Fmt.str "%s\n" (store.uid_to_hex uid)
    | Ref t -> Fmt.str "ref: %s\n" t
  in
  store.atomic_wr t reference str >>| reword_error (fun err -> `Store err)

module type S = sig
  type hash
  type nonrec t = t
  type nonrec contents = hash contents
end

let uid uid = Uid uid
let ref t = Ref t
