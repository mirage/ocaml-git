open Ocamlbuild_plugin

let () = dispatch @@ function
  | After_hygiene ->
    pdep [ "link" ] "linkdep" (fun param -> [ param ])
  | _ -> ()
