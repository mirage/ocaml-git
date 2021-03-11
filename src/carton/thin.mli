open Carton

type ('uid, 's) light_load = 'uid -> (kind * int, 's) io
type ('uid, 's) heavy_load = 'uid -> (Dec.v, 's) io
type optint = Optint.t

module Make
    (Scheduler : SCHEDULER)
    (IO : IO with type 'a t = 'a Scheduler.s)
    (Uid : UID) : sig
  type ('t, 'path, 'fd, 'error) fs = {
    create : ?trunc:bool -> 't -> 'path -> ('fd, 'error) result IO.t;
    append : 't -> 'fd -> string -> unit IO.t;
    map : 't -> 'fd -> pos:int64 -> int -> Bigstringaf.t;
    close : 't -> 'fd -> (unit, 'error) result IO.t;
  }
  (** A record to manipulate a {i file-system}.

      [create] is like {!Unix.openfile}. It can open a pre-existing ['path] or
      create a new one. It returns a {i representation} of it which can be
      manipulated. [append] is like {!Unix.write}. It appends the given
      {i payload} into the opened [fd]. It shifts the offset of the opened file
      by the length of the given {i payload}. [map] is like {!Unix.map_file}. It
      loads a part of the given ['fd] into a {!Bigstringaf.t} payload. It always
      called with valid [pos] and [len] arguments. *)

  val verify :
    ?threads:int ->
    digest:Uid.t Carton.Dec.digest ->
    't ->
    'path ->
    ('t, 'path, 'fd, ([> `Msg of string ] as 'error)) fs ->
    (unit -> (string * int * int) option IO.t) ->
    ( int
      * Uid.t list
      * (int64 * optint) list
      * Uid.t Dec.Idx.entry list
      * int64
      * Uid.t,
      'error )
    result
    IO.t
  (** [verify ~digest filename fs stream] does the first pass to analyze a PACK
      file. While it analyzes the PACK file, it saves it into [filename] with
      the [fs]'s [append] {i syscall}. Then, it returns how many objects has the
      stream, the list of required external objects and the size of the stream.

      If the list is empty, the given stream (saved into [filename]) is a
      {i canonic} PACK file. It does not require any external objects to extract
      any of its objects.

      Otherwise, you probably should call {!canonicalize} to regenerate the PACK
      file. *)

  type nonrec light_load = (Uid.t, Scheduler.t) light_load
  type nonrec heavy_load = (Uid.t, Scheduler.t) heavy_load

  val canonicalize :
    light_load:light_load ->
    heavy_load:heavy_load ->
    src:'path ->
    dst:'path ->
    't ->
    ('t, 'path, 'fd, ([> `Msg of string ] as 'error)) fs ->
    int ->
    Uid.t list ->
    int64 ->
    (int64 * int64 * Uid.t * Uid.t Dec.Idx.entry list, 'error) result IO.t
  (** [canonicalize ~light_load ~heavy_load ~transmit filename fs n requireds
      weight] generates a new PACK file with required objects [requireds]. It
      puts on the front these objects available with [light_load] and
      [heavy_load].

      Then, it transmits all others objects of the old PACK file to the new one
      with [transmit]. It must know how many objects has the old PACK file and
      size of it.

      It returns the size of the new PACK file generated located to [filename]. *)
end
