open Ocamlbuild_plugin

let headers =
  [ "crc32_stubs.h" ]

let () = dispatch @@ function
  | After_hygiene ->
    dep ["compile"; "c"] headers;

    pdep [ "link" ] "linkdep" (fun param -> [ param ])
  | _ -> ()
