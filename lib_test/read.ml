open GitTypes

let () =
  let t = GitLocal.create () in
  GitLocal.dump t;
  GitGraph.to_dot t "graph.dot"
