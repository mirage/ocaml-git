module H = Hashtbl.Make(struct type t = Fpath.t let equal = Fpath.equal let hash = Hashtbl.hash end)

type +'a io = 'a Lwt.t

type t = elt H.t
and key = H.key
and elt = Lwt_mutex.t

let locks = H.create 24

let make locks' path =
  assert (locks == locks');

  try H.find locks path
  with Not_found ->
    let m = Lwt_mutex.create () in
    H.add locks path m;
    m

let remove path =
  H.remove locks path |> Lwt.return (* weak table? *)

let lock m = Lwt_mutex.lock m
let unlock m = Lwt_mutex.unlock m |> Lwt.return

let with_lock m f =
  match m with
  | None -> f ()
  | Some m -> Lwt_mutex.with_lock m f
