let payloads = ref None
let set_payloads (v : string Queue.t) = payloads := Some v

type error = |

let pp_error : error Fmt.t = fun _ppf -> function _ -> .

let get ~ctx:_ ?headers:_ _uri =
  match !payloads with
  | Some queue ->
      let v = Queue.pop queue in
      Lwt.return_ok ((), v)
  | _ -> assert false

let post ~ctx:_ ?headers:_ _uri _contents =
  match !payloads with
  | Some queue ->
      let v = Queue.pop queue in
      Lwt.return_ok ((), v)
  | _ -> assert false
