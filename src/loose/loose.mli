type ('a, 's) io = ('a, 's) Carton.io

module type UID = sig
  include Carton.UID

  val to_hex : t -> string
end

type kind = [ `A | `B | `C | `D ]

type ('t, 'brk, 'error, 's) store = {
  map : 't -> 'brk -> pos:int64 -> int -> (Bigstringaf.t, 's) io;
  (* open + mmap + close
     XXX(dinosaure): according POSIX, close a fd used to [mmap] does not [unmmap]. *)
  mem : 't -> 'brk -> (bool, 's) io;
  (* stat *)
  list : 't -> ('brk list, 's) io;
  (* readdir & closedir *)
  append : 't -> 'brk -> Bigstringaf.t -> ((unit, 'error) result, 's) io;
  (* open + write + close *)
  appendv : 't -> 'brk -> Bigstringaf.t list -> ((unit, 'error) result, 's) io;
      (* open + writev + close *)
}

type buffers = {
  window : De.window;
  lz : De.Lz77.window;
  queue : De.Queue.t;
  i : De.bigstring;
  o : De.bigstring;
  hdr : Cstruct.t;
}

module Make (Uid : UID) : sig
  val list : 't -> ('t, Uid.t, _, 's) store -> (Uid.t list, 's) io
  val exists : 't -> ('t, Uid.t, _, 's) store -> Uid.t -> (bool, 's) io

  val atomic_add :
    's Carton.scheduler ->
    't ->
    buffers ->
    ('t, Uid.t, 'error, 's) store ->
    hdr:(buffer:Cstruct.t -> Carton.Dec.v -> Cstruct.t) ->
    Carton.Dec.v ->
    ((Uid.t * int, [> `Store of 'error | `Non_atomic ]) result, 's) io

  val add :
    's Carton.scheduler ->
    't ->
    buffers ->
    ('t, Uid.t, 'error, 's) store ->
    hdr:Cstruct.t ->
    (unit -> (string option, 's) io) ->
    ((Uid.t * int, [> `Store of 'error ]) result, 's) io

  val atomic_get :
    's Carton.scheduler ->
    't ->
    buffers ->
    ('t, Uid.t, 'error, 's) store ->
    hdr:(Cstruct.t -> Cstruct.t * kind * int64) ->
    Uid.t ->
    ((Carton.Dec.v, [> `Non_atomic ]) result, 's) io

  val size_and_kind :
    's Carton.scheduler ->
    't ->
    buffers ->
    ('t, Uid.t, 'error, 's) store ->
    hdr:(Cstruct.t -> Cstruct.t * kind * int64) ->
    Uid.t ->
    ((int64 * kind, [> `Malformed ]) result, 's) io

  val get :
    's Carton.scheduler ->
    't ->
    buffers ->
    ('t, Uid.t, 'error, 's) store ->
    hdr:(Cstruct.t -> Cstruct.t * kind * int64) ->
    Uid.t ->
    ((Carton.Dec.v, [> `Msg of string ]) result, 's) io
end
