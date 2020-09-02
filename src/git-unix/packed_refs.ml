let src =
  Logs.Src.create "git-unix.packed-refs"
    ~doc:"logs git-unix's packed-refs event"

module Log = (val Logs.src_log src : Logs.LOG)
module Unix_scheduler = Carton.Make (struct type 'a t = 'a end)

let scheduler =
  let open Unix_scheduler in
  { Carton.bind = (fun x f -> f (prj x)); Carton.return = (fun x -> inj x) }

let input_line ic =
  match Stdlib.input_line ic with
  | line -> Unix_scheduler.inj (Some line)
  | exception End_of_file -> Unix_scheduler.inj None

let load ~of_hex dotgit =
  try
    let ic = open_in_bin Fpath.(to_string (dotgit / "packed-refs")) in
    let rs = Git.Reference.Packed.load scheduler ~input_line ~of_hex ic in
    close_in ic;
    Unix_scheduler.prj rs
  with exn ->
    Log.warn (fun m ->
        m "Got an error when we tried to load the packed-refs: %S."
          (Printexc.to_string exn));
    []
