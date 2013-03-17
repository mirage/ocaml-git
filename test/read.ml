open Lib
open Git.Model
open Git.Backend

let () =
  let store = Git.Store.create ".git" in
  let t = Git.Store.read store in
  dump t;
  Git.Algo.to_dot t (File.Name.of_string "graph.dot")
