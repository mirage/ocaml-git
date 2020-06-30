module Mutex = struct
  type 'a fiber = 'a Lwt.t

  type t = Lwt_mutex.t

  let create () = Lwt_mutex.create ()

  let lock t = Lwt_mutex.lock t

  let unlock t = Lwt_mutex.unlock t
end

module Future = struct
  type 'a fiber = 'a Lwt.t

  type 'a t = 'a Lwt.t

  let wait x = x

  let peek x =
    match Lwt.state x with
    | Lwt.Sleep | Lwt.Fail _ -> None
    | Lwt.Return v -> Some v
end

module Condition = struct
  type 'a fiber = 'a Lwt.t

  type mutex = Mutex.t

  type t = unit Lwt_condition.t

  let create () = Lwt_condition.create ()

  let wait t mutex = Lwt_condition.wait ~mutex t

  let signal t = Lwt_condition.signal t ()

  let broadcast t = Lwt_condition.broadcast t ()
end

type 'a t = 'a Lwt.t

let bind x f = Lwt.bind x f

let return x = Lwt.return x

let nfork_map : 'a list -> f:('a -> 'b t) -> 'b t list t =
 fun l ~f -> Lwt.return (List.rev (List.rev_map f l))

let all_unit l = Lwt.join l
