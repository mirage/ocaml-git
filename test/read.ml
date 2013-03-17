open Lib
open Git.Model
open Git.Backend

let read (hex, file) =
  Printf.printf "Reading %s (%s)\n%!"
    (File.Name.to_string file)
    (Hex.Object.to_string hex);
  let x = Object.of_string file (File.read file) in
  Object.dump x

let () =
  let store = Git.Store.create ".git" in
  let objects = Git.Store.objects store in
  List.iter read objects
