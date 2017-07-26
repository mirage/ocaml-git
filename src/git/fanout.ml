module type KEY =
sig
  type t

  val compare : t -> t -> int
  val get     : t -> int -> char
end

module Make (Key : KEY) =
struct
  type 'a t =
    { fanout : (Key.t * 'a) list array }

  let make () =
    { fanout = Array.make 256 [] }

  (* XXX(dinosaure): if, one day, we find a bug about the serialization of the IDX file,
                     may be it's about this function (stable/unstable sort). *)
  let merge lst key value =
    List.merge
      (fun (k1, _) (k2, _) -> Key.compare k1 k2)
      lst [ (key, value) ]

  let bind key value { fanout } =
    let idx = Char.code (Key.get key 0) in
    Array.set fanout idx (merge (Array.get fanout idx) key value)

  let length idx { fanout } =
    if idx < 256
    then List.length (Array.get fanout idx)
    else raise (Invalid_argument "Fanout.fanout")

  let get idx { fanout } =
    if idx < 256
    then Array.get fanout idx
    else raise (Invalid_argument "Fanout.get")
end
