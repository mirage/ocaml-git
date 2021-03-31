(* (c) Frédéric Bour
 * (c) Romain Calascibetta
 *)

module Tbl = struct
  (* XXX(dinosaure): [Tbl] is a small re-implementation
   * of [Hashtbl] where [find_all] is needed by [prj]. To
   * avoid an allocation of an intermediate list, we directly
   * use the underlying linked-list to do the projection.
   *
   * This implementation wants to be:
   * - deterministic (seed = 0)
   * - fast
   *
   * Memoization is done by [last_k]/[last_v] where the common use
   * of [Conduit] is a loop with multiple calls of [send]/[recv]
   * with the same [flow] value.
   *)

  type 'v t = {
    mutable size : int;
    mutable data : 'v lst array;
    mutable last_k : int;
    mutable last_v : 'v;
  }

  and 'v lst = Empty | Cons of { key : int; data : 'v; mutable next : 'v lst }

  let rec power_2_above x n =
    if x >= n then x
    else if x * 2 > Sys.max_array_length then x
    else power_2_above (x * 2) n

  let create ~epsilon size =
    let size = power_2_above 16 size in
    { size = 0; data = Array.make size Empty; last_k = 0; last_v = epsilon }

  external caml_hash : int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]

  let hash v = caml_hash 10 100 0 v

  let resize t =
    let old_data = t.data in
    let old_size = Array.length old_data in
    let new_size = old_size * 2 in
    if new_size < Sys.max_array_length then (
      let new_data = Array.make new_size Empty in
      let new_data_tail = Array.make new_size Empty in
      t.data <- new_data;
      let rec insert = function
        | Empty -> ()
        | Cons { key; next; _ } as cell ->
            let new_idx = hash key land (new_size - 1) in
            (match new_data_tail.(new_idx) with
            | Empty -> new_data.(new_idx) <- cell
            | Cons tail -> tail.next <- cell);
            new_data_tail.(new_idx) <- cell;
            insert next
      in
      for i = 0 to old_size - 1 do
        insert old_data.(i)
      done;
      for i = 0 to new_size - 1 do
        match new_data_tail.(i) with
        | Empty -> ()
        | Cons tail -> tail.next <- Empty
      done)

  let add t key data =
    let i = hash key land (Array.length t.data - 1) in
    let v = Cons { key; data; next = t.data.(i) } in
    t.data.(i) <- v;
    t.size <- t.size + 1;
    if t.size > Array.length t.data lsl 1 then resize t
end

module type KEY_INFO = sig
  type 'a t
end

module Make (Key_info : KEY_INFO) = struct
  type t = ..
  type 'a key = 'a Key_info.t

  module type WITNESS = sig
    type a
    type t += T of a

    val key : a key
  end

  type 'a witness = (module WITNESS with type a = 'a)
  type pack = Key : 'a key -> pack
  type value = Value : 'a * 'a key -> value

  let epsilon _ = raise_notrace Not_found
  let handlers = Tbl.create ~epsilon 0x10
  let keys = Hashtbl.create 0x10

  module Injection (M : sig
    type t

    val key : t key
  end) : WITNESS with type a = M.t = struct
    type a = M.t
    type t += T of a

    let key = M.key
    let handler = function T a -> Value (a, key) | _ -> raise Not_found

    let () =
      let[@warning "-3"] uid =
        Stdlib.Obj.extension_id [%extension_constructor T]
      in
      Tbl.add handlers uid handler;
      Hashtbl.add keys uid (Key key)
  end

  let inj (type a) (key : a key) : a witness =
    (module Injection (struct
      type t = a

      let key = key
    end))

  (* XXX(dinosaure): we ensure that a value [t : t] must have an implementation
   * availble into [handlers]. By this way,
   * [let[@warning "-8"] Tbl.Cons _ = lst in] is safe where we must find an
   * implementation.
   *)

  let rec iter t uid lst =
    let[@warning "-8"] (Tbl.Cons { key = k; data = f; next = r; _ }) = lst in
    try
      if uid <> k then raise_notrace Not_found;
      handlers.Tbl.last_v <- f;
      f t
    with _ -> (iter [@tailcall]) t uid r

  let prj t =
    let arr = handlers.Tbl.data in
    let uid =
      Stdlib.Obj.((extension_id (extension_constructor t) [@warning "-3"]))
    in
    if handlers.Tbl.last_k == uid then handlers.Tbl.last_v t
    else
      let res = iter t uid arr.(Tbl.hash uid land (Array.length arr - 1)) in
      handlers.Tbl.last_k <- uid;
      res

  let bindings () = Hashtbl.fold (fun _ v a -> v :: a) keys []
end
