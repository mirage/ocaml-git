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

  let handlers = Hashtbl.create 0x10
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
      Hashtbl.add handlers uid handler;
      Hashtbl.add keys uid (Key key)
  end

  let inj (type a) (key : a key) : a witness =
    (module Injection (struct
      type t = a

      let key = key
    end))

  let rec iter t = function
    | [] -> assert false
    | hd :: tl -> ( try hd t with Not_found -> iter t tl)

  let prj t =
    let uid =
      Stdlib.Obj.((extension_id (extension_constructor t) [@warning "-3"]))
    in
    iter t (Hashtbl.find_all handlers uid)

  let bindings () = Hashtbl.fold (fun _ v a -> v :: a) keys []
end
