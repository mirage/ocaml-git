open Lib

let read (hex, file) =
  Printf.printf "Reading %s (%s)\n%!"
    (File.Name.to_string file)
    (Git.Model.Object.Hex.to_string hex);
  let x = Git.Model.Object.C.of_string file (File.read file) in
  Git.Model.Object.C.dump x

let () =
  let store = Git.Store.create ".git" in
  let objects = Git.Store.objects store in
  List.iter read objects
