open Astring

type t =
  [ `Atom of string
  | `Key_value of string * string
  | `Command_features of string * string list ]

(* TODO: integrate better support for known capabilities and commands
   e.g., ls-refs, etc. *)
let of_string s =
  match String.cut ?rev:None ~sep:"=" s with
  | None -> `Atom s
  | Some (k, v) -> (
      match String.cuts ?rev:None ?empty:None ~sep:" " v with
      | [] -> raise @@ Invalid_argument s
      | [ v ] -> `Key_value (k, v)
      | command :: features -> `Command_features (command, features))

let to_string = function
  | `Atom s -> s
  | `Key_value (k, v) -> Fmt.str "%s=%s" k v
  | `Command_features (s, s_lst) ->
      Fmt.str "%s=%s" s (String.concat ~sep:" " s_lst)

let equal t1 t2 =
  match t1, t2 with
  | `Atom s1, `Atom s2 -> String.equal s1 s2
  | `Key_value (k1, v1), `Key_value (k2, v2) ->
      String.equal k1 k2 && String.equal v1 v2
  | `Command_features (c1, fs1), `Command_features (c2, fs2) ->
      String.equal c1 c2 && List.for_all2 String.equal fs1 fs2
  | _ -> false
