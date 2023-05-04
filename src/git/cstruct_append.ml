(* XXX(dinosaure): this implementation is specialised to be used by
   [Git.Mem] and [Smart_git]. So we can rely on one assumption, we will
   create only 2 "objects"/"files":
   - a temporary object
   - the destination object

   In this context, [t] can stores only 2 objects. We should extend the
   implementation to be more general but the trade-off is bad. *)

let src = Logs.Src.create "git-cstruct-append"

module Log = (val Logs.src_log src : Logs.LOG)

type 'a rd = < rd : unit ; .. > as 'a
type 'a wr = < wr : unit ; .. > as 'a

type 'a mode =
  | Rd : < rd : unit > mode
  | Wr : < wr : unit > mode
  | RdWr : < rd : unit ; wr : unit > mode

type uid = < >

module Ephemeron = Ephemeron.K1.Make (struct
  type t = uid

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type t = { storage : (bool * Cstruct.t ref) Ephemeron.t; mutable which : bool }
type +'a fiber = 'a Lwt.t
type error = |

let pp_error : error Fmt.t = fun _ppf -> function _ -> .
let device () = { storage = Ephemeron.create 2; which = true }
let empty = Cstruct.create 0

let key device =
  let file = object end in
  Ephemeron.add device.storage file (device.which, ref empty);
  device.which <- not device.which;
  file

type 'a fd = {
  mutable buffer : Cstruct.t;
  mutable capacity : int;
  mutable length : int;
  uid : Ephemeron.key;
}

let enlarge fd more =
  Log.debug (fun m ->
      m "Start to enlarge the given buffer (+ %d byte(s))." more);
  let _old_length = fd.length in
  let old_capacity = fd.capacity in
  let new_capacity = ref old_capacity in
  Log.debug (fun m ->
      m "Current capacity of the given buffer: %d byte(s)." old_capacity);
  while old_capacity + more > !new_capacity do
    new_capacity := 2 * !new_capacity
  done;
  Log.debug (fun m ->
      m "old capacity: %d, new capacity: %d." old_capacity !new_capacity);
  if !new_capacity > Sys.max_string_length then
    if old_capacity + more <= Sys.max_string_length then
      new_capacity := Sys.max_string_length
    else failwith "Too big buffer";
  let new_buffer = Cstruct.create !new_capacity in
  Cstruct.blit fd.buffer 0 new_buffer 0 fd.length;
  fd.buffer <- new_buffer;
  fd.capacity <- !new_capacity;
  (* XXX(dinosaure): these asserts wants to rely on some assumptions
     even if we use [enlarge] into a preemptive thread as [Stdlib.Buffer].
     However, with [lwt], it should be fine to use it and avoid these
     assertions. *)
  (* assert (fd.position + more <= fd.capacity) ; *)
  (* assert (old_length + more <= fd.capacity) ; *)
  ()

(* XXX(dinosaure): use [Cstruct_cap]? I think we must prove capabilities
 * with [Refl]. *)
let create ?(trunc = true) ~mode:_ { storage; _ } uid =
  let which, value = Ephemeron.find storage uid in

  let value =
    if Cstruct.length !value < 1 then (
      let v = Cstruct.create 1 in
      value := v;
      v)
    else !value
  in

  Log.debug (fun m ->
      m "Make a new file-descriptor (%b) (%d byte(s))." which
        (Cstruct.length value));
  let fd =
    {
      buffer = value;
      capacity = Cstruct.length value;
      length = (if trunc then 0 else Cstruct.length value);
      uid;
    }
  in
  Lwt.return_ok fd

let append _ fd str =
  let len = String.length str in
  let new_length = fd.length + len in
  if new_length > fd.capacity then enlarge fd len;
  Cstruct.blit_from_string str 0 fd.buffer fd.length len;
  fd.length <- new_length;
  Log.debug (fun m -> m "Append + %d byte(s)." fd.length);
  Lwt.return ()

let map _ fd ~pos len =
  Log.debug (fun m -> m "map on fd(length:%d) ~pos:%Ld %d." fd.length pos len);
  let pos = Int64.to_int pos in
  if pos > Cstruct.length fd.buffer then Bigstringaf.empty
  else
    let len = min len (Cstruct.length fd.buffer - pos) in
    let { Cstruct.buffer; off; _ } = fd.buffer in
    let res = Bigstringaf.sub ~off:(off + pos) ~len buffer in
    res

let close device fd =
  let result = Cstruct.sub fd.buffer 0 fd.length in
  Log.debug (fun m ->
      m "Close the object into the cstruct-append heap (save %d bytes)"
        fd.length);
  let _, cell = Ephemeron.find device.storage fd.uid in
  cell := result;
  Lwt.return_ok ()

let move device ~src ~dst =
  Log.debug (fun m -> m "Start to move a key to another.");
  if src == dst then Lwt.return_ok ()
  else
    let a, srcv = Ephemeron.find device.storage src in
    let b, dstv = Ephemeron.find device.storage dst in
    assert (a <> b);
    let tmpv = !srcv in
    srcv := !dstv;
    dstv := tmpv;
    Lwt.return_ok ()

let project device uid =
  let _, cell = Ephemeron.find device.storage uid in
  !cell
