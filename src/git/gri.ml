type t = Uri.t

let uri x = x

let host uri =
  match Uri.host uri with
  | Some host -> host
  | None -> Fmt.invalid_arg "Invalid gri: no host"

let path uri = Uri.path_and_query uri
let pp ppf x = Fmt.string ppf (Uri.to_string x)

let make : Uri.t -> t =
 fun uri ->
  match Uri.scheme uri, Uri.host uri with
  | Some "git", Some _ -> uri
  | Some scheme, _ -> Fmt.invalid_arg "Invalid gri: invalid scheme (%s)" scheme
  | _, None -> Fmt.invalid_arg "Invalid gri: no host"
  | None, Some _ -> Fmt.invalid_arg "Invalid gri: no scheme"

let of_string x = make (Uri.of_string x)
