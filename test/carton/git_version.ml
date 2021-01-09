type t = {
  major : int;
  minor : int;
  patch : string option;
  revision : int option;
  release_candidate : string option;
}

let pp ppf t =
  match t.patch, t.revision, t.release_candidate with
  | None, None, None -> Format.fprintf ppf "v%d.%d" t.major t.minor
  | Some p, None, None -> Format.fprintf ppf "v%d.%d.%s" t.major t.minor p
  | Some p, Some rev, None ->
      Format.fprintf ppf "v%d.%d.%s.%d" t.major t.minor p rev
  | Some p, Some rev, Some rc ->
      Format.fprintf ppf "v%d.%d.%s.%d-%s" t.major t.minor p rev rc
  | None, Some rev, Some rc ->
      Format.fprintf ppf "v%d.%d.%d-%s" t.major t.minor rev rc
  | None, None, Some rc ->
      if t.major = 1 && t.minor = 0 then
        Format.fprintf ppf "v%d.%d%s" t.major t.minor rc
      else Format.fprintf ppf "v%d.%d-%s" t.major t.minor rc
  | None, Some rev, None -> Format.fprintf ppf "v%d.%d.%d" t.major t.minor rev
  | Some p, None, Some rc ->
      Format.fprintf ppf "v%d.%d.%s-%s" t.major t.minor p rc

let compare a b =
  let a = Format.asprintf "%a" pp a in
  let b = Format.asprintf "%a" pp b in
  String.compare a b

let make mj mn patch revision rc =
  let release_candidate = if rc = "" then None else Some rc in
  Some { major = mj; minor = mn; patch; revision; release_candidate }

let parse str =
  try
    Scanf.sscanf str "git version %d.%d.%d.%d%1[-]%s"
      (fun mj mn patch rev _ rc ->
        make mj mn (Some (string_of_int patch)) (Some rev) rc)
  with _ -> (
    try
      Scanf.sscanf str "git version %d.%d.%d%1[-]%s" (fun mj mn patch dash rc ->
          if dash = "-" then make mj mn (Some (string_of_int patch)) None rc
          else make mj mn (Some (string_of_int patch ^ rc)) None "")
    with _ -> (
      try
        Scanf.sscanf str "git version %d.%d-%s" (fun mj mn rc ->
            make mj mn None None rc)
      with _ -> (
        try
          Scanf.sscanf str "git version %d.%d%s" (fun mj mn rc ->
              make mj mn None None rc)
        with _ -> None)))
