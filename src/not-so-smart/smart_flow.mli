open Sigs

val io_buffer_size : int

type ('a, 's) raise = exn -> ('a, 's) Sigs.io

val run :
  's scheduler ->
  ('res, 's) raise ->
  ('flow, 'error, 's) flow ->
  'flow ->
  ('res, [ `Protocol of Smart.error ]) Smart.t ->
  ('res, 's) io
