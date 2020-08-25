type encoder = { payload : Bytes.t; mutable pos : int }

val io_buffer_size : int

val encoder : unit -> encoder

type error = [ `No_enough_space ]

val pp_error : error Fmt.t

type 'err state =
  | Write of {
      buffer : string;
      off : int;
      len : int;
      continue : int -> 'err state;
    }
  | Error of 'err
  | Done

val safe : (encoder -> ([> error ] as 'err) state) -> encoder -> 'err state

val flush : (encoder -> ([> error ] as 'err) state) -> encoder -> 'err state

val write : encoder -> string -> unit

val blit : encoder -> buf:string -> off:int -> len:int -> unit
