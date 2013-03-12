let objects dir =
  let objects = Filename.concat dir "objects" in
  let subdirs = Sys.readdir objects in
  let subdirs = List.map (Filename.concat objects) (Array.to_list subdirs) in
  let objects =
    List.map (fun dir ->
      let files = Sys.readdir dir in
      let files = Array.to_list files in
      List.map (Filename.concat dir) files
    ) subdirs in
  List.flatten objects

let read f =
  Printf.printf "Reading %s\n%!" f;
  let x = Git.Object.of_string ~file:f (Git.read_file f) in
  Git.Object.dump x

let () =
  let objects = objects ".git" in
  List.iter read objects
