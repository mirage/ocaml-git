(* Copyright 2014 Citrix - ISC licensed *)

val cstruct: ?crc:int32 -> Cstruct.t -> int32
(** [cstruct ?crc buf] computes the CRC of [buf] with optional
    initial value [crc] *)

val string: ?crc:int32 -> string -> int -> int -> int32
(** [string ?crc buf ofs len] computes the CRC of the substring
    of length [len] starting at offset [ofs] in string [buf] with
    optional initial value [crc] *)
