open Lib
open Git.Model
open Git.Backend

let () =
  let t = Git.Store.create (File.Dirname.of_string ".git") in
  Git.Store.dump t;
  Git.View.to_dot t (File.Name.of_string "graph.dot")
