module type STORE = sig
  type 'a rd = < rd : unit ; .. > as 'a
  type 'a wr = < wr : unit ; .. > as 'a

  type 'a mode =
    | Rd : < rd : unit > mode
    | Wr : < wr : unit > mode
    | RdWr : < rd : unit ; wr : unit > mode

  type t
  type uid
  type 'a fd
  type error
  type +'a fiber

  val pp_error : error Fmt.t
  val create : mode:'a mode -> t -> uid -> ('a fd, error) result fiber
  val map : t -> 'm rd fd -> pos:int64 -> int -> Bigstringaf.t
  val close : t -> 'm fd -> (unit, error) result fiber
  val list : t -> uid list fiber
  val length : 'm fd -> int64 fiber
end

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

type ('fd, 'uid) pack = {
  pack : ('fd * int64, 'uid) Carton.Dec.t;
  index : 'uid Carton.Dec.Idx.idx;
  z : Bigstringaf.t;
  w : De.window;
}

type ('path, 'fd, 'uid) t = { tbl : ('path, ('fd, 'uid) pack) Hashtbl.t }
[@@unbox]

type 'fd buffers = {
  z : Bigstringaf.t;
  allocate : int -> De.window;
  w : 'fd Carton.Dec.W.t;
}

module Make
    (Scheduler : Carton.SCHEDULER)
    (IO : IO with type +'a t = 'a Scheduler.s)
    (Store : STORE with type +'a fiber = 'a Scheduler.s)
    (Uid : Carton.UID) =
struct
  let ( >>= ) = IO.bind
  let return = IO.return
  let ( >>? ) x f = x >>= function Ok x -> f x | Error _ as err -> return err
  let ( >>| ) x f = x >>= fun x -> return (f x)

  let idx (root : Store.t) acc path =
    Store.create ~mode:Store.Rd root path >>? fun fd ->
    Store.length fd >>= fun length ->
    let payload = Store.map root fd ~pos:0L (Int64.to_int length) in
    Store.close root fd >>? fun () ->
    let idx =
      Carton.Dec.Idx.make payload ~uid_ln:Uid.length ~uid_rw:Uid.to_raw_string
        ~uid_wr:Uid.of_raw_string
    in
    return (Ok (idx :: acc))

  let pack (root : Store.t) acc (index, pack) =
    Store.create ~mode:Store.Rd root pack >>? fun fd ->
    Store.length fd >>= fun length ->
    let z = Bigstringaf.create De.io_buffer_size in
    let w = De.make_window ~bits:15 in
    let pack =
      Carton.Dec.make (fd, length) ~z
        ~allocate:(fun _ -> w)
        ~uid_ln:Uid.length ~uid_rw:Uid.of_raw_string
        (fun uid ->
          match Carton.Dec.Idx.find index uid with
          | Some (_, offset) -> offset
          | None -> Fmt.invalid_arg "Object %a does not exist" Uid.pp uid)
    in
    return (Ok ({ pack; index; z; w } :: acc))

  let fold_left_r ?(err = fun _ -> return ()) f a l =
    let rec go a = function
      | [] -> return a
      | x :: r -> (
          f a x >>= function
          | Ok a -> go a r
          | Error x -> err x >>= fun () -> go a r)
    in
    go a l

  let ( <.> ) f g x = f (g x)

  (* XXX(dinosaure): about design, I think that a listing of PACK files should be done
     outside the scope of this module (or more generally outside the scope of the Git's core). *)
  let make :
      Store.t ->
      uid_of_major_uid:(Store.uid -> 'uid) ->
      idx_major_uid_of_uid:(Store.t -> 'uid -> Store.uid) ->
      (Store.uid, < rd : unit > Store.fd, Uid.t) t IO.t =
   fun root ~uid_of_major_uid ~idx_major_uid_of_uid ->
    Store.list root >>= fun pcks ->
    let idxs = List.map (idx_major_uid_of_uid root <.> uid_of_major_uid) pcks in
    fold_left_r (idx root) [] idxs >>| List.rev >>= fun idxs ->
    fold_left_r (pack root) [] (List.combine idxs pcks) >>| List.rev
    >>= fun vs ->
    let tbl = Hashtbl.create 10 in
    List.iter (fun (k, v) -> Hashtbl.add tbl k v) (List.combine pcks vs);
    return { tbl }

  let map root (fd, top) ~pos len =
    let max = Int64.sub top pos in
    let len = min (Int64.of_int len) max in
    let len = Int64.to_int len in
    Store.map root fd ~pos len

  let add :
      Store.t ->
      (Store.uid, < rd : unit > Store.fd, Uid.t) t ->
      idx:Store.uid ->
      Store.uid ->
      (unit, Store.error) result IO.t =
   fun root p ~idx:idx_uid pck ->
    idx root [] idx_uid >>? fun idxs ->
    let[@warning "-8"] [ idx ] = idxs in
    pack root [] (idx, pck) >>? fun vs ->
    List.iter (fun (k, v) -> Hashtbl.add p.tbl k v) (List.combine [ pck ] vs);
    return (Ok ())

  let with_resources root pack uid buffers =
    let map fd ~pos len = map root fd ~pos len in
    let pack = Carton.Dec.with_z buffers.z pack in
    let pack = Carton.Dec.with_allocate ~allocate:buffers.allocate pack in
    let pack = Carton.Dec.with_w buffers.w pack in
    let weight =
      Carton.Dec.weight_of_uid ~map pack ~weight:Carton.Dec.null uid
    in
    let raw = Carton.Dec.make_raw ~weight in
    let v = Carton.Dec.of_uid ~map pack raw uid in
    return v

  let get :
      Store.t ->
      resources:('fd -> ('fd buffers -> 'a IO.t) -> 'a IO.t) ->
      (Store.uid, < rd : unit > Store.fd, Uid.t) t ->
      Uid.t ->
      (Carton.Dec.v, [> `Msg of string ]) result IO.t =
   fun root ~resources p uid ->
    let res = ref None in
    Hashtbl.iter
      (fun k { index; _ } ->
        if Carton.Dec.Idx.exists index uid then res := Some k)
      p.tbl;
    match !res with
    | Some k ->
        let { pack; _ } = Hashtbl.find p.tbl k in
        resources (Carton.Dec.fd pack) (with_resources root pack uid)
        >>= fun v -> return (Ok v)
    | None -> return (Error (`Not_found uid))

  let list : Store.t -> (Store.uid, 'm Store.fd, Uid.t) t -> Uid.t list =
   fun _ p ->
    let fold _ { index; _ } a =
      let res = ref [] in
      Carton.Dec.Idx.iter
        ~f:(fun ~uid ~offset:_ ~crc:_ -> res := uid :: !res)
        index;
      List.rev_append !res a
    in
    Hashtbl.fold fold p.tbl []

  let exists : Store.t -> (Store.uid, 'm Store.fd, Uid.t) t -> Uid.t -> bool =
   fun _ p uid ->
    let res = ref false in
    Hashtbl.iter
      (fun _ { index; _ } ->
        if Carton.Dec.Idx.exists index uid then res := true)
      p.tbl;
    !res

  let fds : (Store.uid, 'm Store.fd, Uid.t) t -> ('m Store.fd * int64) list =
   fun { tbl } ->
    let fold _ { pack; _ } a = Carton.Dec.fd pack :: a in
    Hashtbl.fold fold tbl []
end
