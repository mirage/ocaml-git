let src = Logs.Src.create "git.loose" ~doc:"logs git's loose event"

module Log = (val Logs.src_log src : Logs.LOG)

module type STORE = sig
  type t
  type uid
  type error
  type +'a fiber

  val pp_error : error Fmt.t
  val exists : t -> uid -> bool fiber
  val length : t -> uid -> (int64, error) result fiber
  val map : t -> uid -> pos:int64 -> int -> Bigstringaf.t fiber
  val append : t -> uid -> Bigstringaf.t -> (unit, error) result fiber
  val appendv : t -> uid -> Bigstringaf.t list -> (unit, error) result fiber
  val list : t -> uid list fiber
  val reset : t -> (unit, error) result fiber
end

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
end

module Make
    (Scheduler : Carton.SCHEDULER)
    (IO : IO with type +'a t = 'a Scheduler.s)
    (Store : STORE with type +'a fiber = 'a Scheduler.s)
    (Uid : Loose.UID with type t = Store.uid) =
struct
  let ( >>= ) = IO.bind
  let return = IO.return

  let io =
    let open Scheduler in
    {
      Carton.bind = (fun x f -> inj (prj x >>= fun x -> prj (f x)));
      Carton.return = (fun x -> inj (return x));
    }

  (* XXX(dinosaure): at this layer, [loose] can ask something bigger than
     the length of [uid]. So we fix it with this process and return as
     much as we can. *)
  let store_map root uid ~pos len =
    if pos < 0L || len < 0 then invalid_arg "store_map: invalid bounds";
    Store.length root uid >>= function
    | Error _ -> return Bigstringaf.empty
    | Ok max ->
        let len = min (Int64.add pos (Int64.of_int len)) (Int64.sub max pos) in
        let len = Int64.to_int len in
        Store.map root uid ~pos len

  let store_mem root uid = Store.exists root uid
  let store_append root uid payload = Store.append root uid payload
  let store_appendv root uid payloads = Store.appendv root uid payloads
  let store_list root = Store.list root

  let store =
    {
      Loose.map =
        (fun t uid ~pos len -> Scheduler.inj (store_map t uid ~pos len));
      Loose.mem = (fun t uid -> Scheduler.inj (store_mem t uid));
      Loose.append = (fun t uid v -> Scheduler.inj (store_append t uid v));
      Loose.appendv = (fun t uid vs -> Scheduler.inj (store_appendv t uid vs));
      Loose.list = (fun t -> Scheduler.inj (store_list t));
    }

  let space = Cstruct.of_string " "
  let zero = Cstruct.of_string "\000"

  (* TODO(dinosaure): integrate it into [cstruct]. *)
  let cut ~sep:({ Cstruct.len = sep_len; _ } as sep) ({ Cstruct.len; _ } as t) =
    if sep_len = 0 then invalid_arg "cut: empty separator";
    let max_sep_zidx = sep_len - 1 in
    let max_t_zidx = len - sep_len in
    let rec check_sep i k =
      if k > max_sep_zidx then
        Some (Cstruct.sub t 0 i, Cstruct.sub t (i + sep_len) (len - sep_len - i))
      else if Cstruct.get_char t (i + k) = Cstruct.get_char sep k then
        check_sep i (succ k)
      else scan (succ i)
    and scan i =
      if i > max_t_zidx then None
      else if Cstruct.get_char t i = Cstruct.get_char sep 0 then check_sep i 1
      else scan (succ i)
    in
    scan 0

  let hdr_get raw =
    match cut ~sep:space raw with
    | None -> failwith "Invalid Git header"
    | Some (kind, rest) -> (
        match cut ~sep:zero rest with
        | Some (length, contents) ->
            let length = Int64.of_string (Cstruct.to_string length) in
            let kind =
              match Cstruct.to_string kind with
              | "commit" -> `A
              | "blob" -> `C
              | "tag" -> `D
              | "tree" -> `B
              | v -> Fmt.failwith "Invalid type of Git object: %s" v
            in
            contents, kind, length
        | None -> failwith "Invalid Git header")

  let hdr_set ~buffer (kind, length) =
    let kind =
      match kind with
      | `Commit -> "commit"
      | `Tree -> "tree"
      | `Blob -> "blob"
      | `Tag -> "tag"
    in
    Cstruct.blit_from_string kind 0 buffer 0 (String.length kind);
    Cstruct.set_char buffer (String.length kind) ' ';
    let length = Int64.to_string length in
    Cstruct.blit_from_string length 0 buffer
      (String.length kind + 1)
      (String.length length);
    Cstruct.set_char buffer
      (String.length kind + 1 + String.length length)
      '\000';
    Cstruct.sub buffer 0 (String.length kind + 1 + String.length length + 1)

  include Loose.Make (Uid)

  let list t = Scheduler.prj (store.list t)
  let exists t uid = Scheduler.prj (exists t store uid)

  let atomic_add t buffers v =
    let hdr_set ~buffer v =
      let kind =
        match Carton.Dec.kind v with
        | `A -> `Commit
        | `B -> `Tree
        | `C -> `Blob
        | `D -> `Tag
      in
      let length = Int64.of_int (Carton.Dec.len v) in
      hdr_set ~buffer (kind, length)
    in
    Scheduler.prj (atomic_add io t buffers store ~hdr:hdr_set v)

  let add t buffers (kind, length) stream =
    let hdr = hdr_set ~buffer:(Cstruct.create 30) (kind, length) in
    let stream () = Scheduler.inj (stream ()) in
    Scheduler.prj (add io t buffers store ~hdr stream)

  let atomic_get t buffer uid =
    Scheduler.prj (atomic_get io t buffer store ~hdr:hdr_get uid)

  let size_and_kind t buffers uid =
    Scheduler.prj (size_and_kind io t buffers store ~hdr:hdr_get uid)

  let get t buffer uid = Scheduler.prj (get io t buffer store ~hdr:hdr_get uid)
end
