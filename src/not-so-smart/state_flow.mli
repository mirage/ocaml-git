module Log : Logs.LOG

val io_buffer_size : int

type ('a, 's) raise = exn -> ('a, 's) Sigs.io

module Make : functor
  (Read_write : sig
     type ('a, 'err) t = ('a, 'err) State.t
     type error

     val pp_error : error Fmt.t
   end)
  -> sig
  type nonrec error = Read_write.error

  val run :
    's Sigs.scheduler ->
    ('res, 's) raise ->
    ('fl, 'error, 's) Sigs.flow ->
    'fl ->
    ('res, [ `Protocol of error ]) State.t ->
    ('res, 's) Sigs.io
end
