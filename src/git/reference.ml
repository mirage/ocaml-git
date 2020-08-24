(* (c) 2015 Daniel C. Bünzli
 * (c) 2020 Romain Calascibetta
 *
 * This implementation differs a bit from [fpath]
 * where absolute path is not valid and we manipulate
 * only POSIX path - the backend takes care about Windows
 * and POSIX paths then. *)




type t = string

let of_string x = x

let to_string x = x

let sep = "/"


let head = "HEAD"

let is_head = String.equal head

let master = "refs/heads/master"

let to_path = function "HEAD" -> Path.v "HEAD" | refs -> Path.v refs

let of_path path =
  match Path.segs path with
  | [] -> Fmt.invalid_arg "Reference.of_path: empty path"
  | [ "HEAD" ] -> head
  | "HEAD" :: _ ->
      Fmt.invalid_arg "Reference.of_path: HEAD can not be followed by values"
  | "refs" :: _ as refs -> String.concat sep refs
  | _ :: _ ->
      invalid_arg "Reference.of_path: bad path (need to be prefixed by refs)"

let pp ppf x = Fmt.pf ppf "%s" (String.escaped x)

  type nonrec t = t

end

let compare_contents ~compare:compare_uid a b =
  let inf = -1 and sup = 1 in
  match (a, b) with
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
          | Some _ ->
          match Astring.String.cut ~sep:" " line with
          | Some (uid, reference) ->
              let reference = v reference in
              let uid = of_hex uid in
              go (Ref (reference, uid) :: acc)
          | None -> go acc)
      | None -> return (List.rev acc) in
    go []

  exception Found

  let exists reference packed =
    let res = ref false in
    let f = function
      | Ref (reference', _) ->
          if equal reference reference'
          then (
            res := true ;
            raise Found)
      | _ -> () in
    (try List.iter f packed with Found -> ()) ;
    !res

  let get reference packed =
    let res = ref None in
    let f = function
      | Ref (reference', uid) ->
          if equal reference reference'
          then (
            res := Some uid ;
            raise Found)
      | _ -> () in
    (try List.iter f packed with Found -> ()) ;
    !res

  let remove reference packed =
    let fold acc = function
      | Ref (reference', uid) ->
          if equal reference reference'
          then acc
          else Ref (reference', uid) :: acc
      | v -> v :: acc in
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
      | _ -> Fmt.invalid_arg "Invalid reference contents: %S" str)

let resolve { Carton.bind; Carton.return } t store reference =
  let ( >>= ) = bind in

  let rec go visited reference =
    store.atomic_rd t reference >>= function
    | Error _ -> (
        match Packed.get reference store.packed with
        | Some uid -> return (Ok uid)
        | None -> return (Error (`Not_found reference)))
    | Ok str ->
    match contents store str with
    | Uid uid -> return (Ok uid)
    | Ref reference ->
        if List.exists (equal reference) visited
        then return (Error `Cycle)
        else go (reference :: visited) reference in
  go [ reference ] reference

let read { Carton.bind; Carton.return } t store reference =
  let ( >>= ) = bind in

  store.atomic_rd t reference >>= function
  | Error _ -> (
      match Packed.get reference store.packed with
      | Some uid -> return (Ok (Uid uid))
      | None -> return (Error (`Not_found reference)))
  | Ok str -> return (Ok (contents store str))

let write { Carton.bind; Carton.return } t store reference contents =
  let ( >>= ) = bind in
  let ( >>| ) x f = x >>= fun x -> return (f x) in

  let str =
    match contents with
    | Uid uid -> Fmt.strf "%s\n" (store.uid_to_hex uid)
    | Ref t -> Fmt.strf "ref: %s\n" t in
  store.atomic_wr t reference str >>| reword_error (fun err -> `Store err)


end

