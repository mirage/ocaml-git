type lwt = Carton_lwt.lwt

module Make (Uid : Loose.UID) = struct
  include Loose.Make (Uid)

  let prj = Carton_lwt.prj
  let exists t store uid = prj (exists t store uid)

  let atomic_add t buffers store ~hdr v =
    prj (atomic_add Carton_lwt.lwt t buffers store ~hdr v)

  let add t buffers store ~hdr stream =
    let stream () = Carton_lwt.inj (stream ()) in
    prj (add Carton_lwt.lwt t buffers store ~hdr stream)

  let atomic_get t buffers store ~hdr uid =
    prj (atomic_get Carton_lwt.lwt t buffers store ~hdr uid)

  let size_and_kind t buffers store ~hdr uid =
    prj (size_and_kind Carton_lwt.lwt t buffers store ~hdr uid)

  let get t buffers store ~hdr uid =
    prj (get Carton_lwt.lwt t buffers store ~hdr uid)
end
