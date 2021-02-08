module Log : Logs.LOG

val io_buffer_size : int

type ('a, 's) raise = exn -> ('a, 's) Sigs.io

val run :
  's Sigs.scheduler ->
  ('res, 's) raise ->
  'err Fmt.t ->
  ('fl, 'error, 's) Sigs.flow ->
  'fl ->
  ('res, [ `Protocol of 'err ]) State.t ->
  ('res, 's) Sigs.io
