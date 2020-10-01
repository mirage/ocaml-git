(* XXX(dinosaure): this implementation is specialised to be used by
   [Git.Mem] and [Smart_git]. So we can rely on one assumption, we will
   create only 2 "objects"/"files":
   - a temporary object
   - the destination object

   In this context, [t] can stores only 2 objects. We should extend the
   implementation to be more general but the trade-off is bad. *)

open Stdlib

let src = Logs.Src.create "git-cstruct-append"

module Log = (val Logs.src_log src : Logs.LOG)

type 'a rd = < rd : unit ; .. > as 'a
type 'a wr = < wr : unit ; .. > as 'a

type 'a mode =
  | Rd : < rd : unit > mode
  | Wr : < wr : unit > mode
  | RdWr : < rd : unit ; wr : unit > mode

type t = {
  o0 : (key ref, Cstruct.t ref) Ephemeron.K1.t;
  o1 : (key ref, Cstruct.t ref) Ephemeron.K1.t;
  mutable which : bool;
}

and key = Key

type +'a fiber = 'a Lwt.t
type error = |

let pp_error : error Fmt.t = fun _ppf -> function _ -> .

let device () =
  { o0 = Ephemeron.K1.create (); o1 = Ephemeron.K1.create (); which = false }

let empty = Cstruct.create 0

let key tbl =
  let value = ref Key in
  if tbl.which then (
    Ephemeron.K1.set_key tbl.o0 value;
    Ephemeron.K1.set_data tbl.o0 (ref empty);
    tbl.which <- not tbl.which;
    value )
  else (
    Ephemeron.K1.set_key tbl.o1 value;
    Ephemeron.K1.set_data tbl.o1 (ref empty);
    tbl.which <- not tbl.which;
    value )
  [@@inline never]

type uid = key ref

type 'a fd = {
  mutable buffer : Cstruct.t;
  mutable capacity : int;
  mutable length : int;
  which : bool;
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
let create ~mode:_ { o0; o1; _ } key =
  let which, value =
    let k0 =
      Option.fold ~none:false
        ~some:(fun key' -> key == key')
        (Ephemeron.K1.get_key o0)
    in
    let k1 =
      Option.fold ~none:false
        ~some:(fun key' -> key == key')
        (Ephemeron.K1.get_key o1)
    in
    assert (not (k0 && k1));
    let value =
      if k0 then Option.get (Ephemeron.K1.get_data o0)
      else Option.get (Ephemeron.K1.get_data o1)
    in
    k0, value
  in

  let value =
    if Cstruct.len !value < 1 then (
      let v = Cstruct.create 1 in
      value := v;
      v )
    else !value
  in

  Log.debug (fun m -> m "Make a new file-descriptor (%b)." which) ;
  let fd =
    {
      buffer = value;
      capacity = Cstruct.len value;
      length = 0 (* XXX(dinosaure): like Unix.O_TRUNC *);
      which;
    }
  in
  Lwt.return_ok fd

let append _ fd str =
  let len = String.length str in
  let new_length = fd.length + len in
  if new_length > fd.capacity then enlarge fd len;
  Cstruct.blit_from_string str 0 fd.buffer fd.length len;
  fd.length <- new_length;
  Log.debug (fun m -> m "Append + %d byte(s)." fd.length) ;
  Lwt.return ()

let map _ fd ~pos len =
  Log.debug (fun m -> m "map on fd(length:%d) ~pos:%Ld %d."
                fd.length pos len) ;
  let pos = Int64.to_int pos in
  if pos > fd.length then Lwt.return Bigstringaf.empty
  else
    let len = min len (fd.length - pos) in
    let { Cstruct.buffer; off; _ } = fd.buffer in
    Lwt.return (Bigstringaf.sub ~off:(off + pos) ~len buffer)

let close tbl fd =
  let result = Cstruct.sub fd.buffer 0 fd.length in
  Log.debug (fun m -> m "Close the object into the cstruct-append heap (save %d bytes)." fd.length);
  ( if fd.which then
    match Ephemeron.K1.get_data tbl.o0 with
    | Some value -> value := result
    | None -> assert false
  else
    match Ephemeron.K1.get_data tbl.o1 with
    | Some value -> value := result
    | None -> assert false );
  Lwt.return_ok ()

let move tbl ~src ~dst =
  Log.debug (fun m -> m "Start to move a key to another.");
  if src == dst then Lwt.return_ok ()
  else
    match Ephemeron.K1.get_data tbl.o0, Ephemeron.K1.get_data tbl.o1 with
    | Some v0, Some v1 ->
        let k0 = Option.get (Ephemeron.K1.get_key tbl.o0) in
        let k1 = Option.get (Ephemeron.K1.get_key tbl.o1) in
        if src == k0 && dst == k1 then v1 := !v0
        else if src == k1 && dst == k0 then v0 := !v1
        else (
          Log.err (fun m -> m "Given keys are wrong!");
          assert false );
        Lwt.return_ok ()
    | _ ->
        Log.err (fun m -> m "One object was deleted by the garbage collector.");
        assert false

let project tbl uid =
  if
    Option.fold ~none:false
      ~some:(fun k -> k == uid)
      (Ephemeron.K1.get_key tbl.o0)
  then !(Option.get (Ephemeron.K1.get_data tbl.o0))
  else if
    Option.fold ~none:false
      ~some:(fun k -> k == uid)
      (Ephemeron.K1.get_key tbl.o1)
  then !(Option.get (Ephemeron.K1.get_data tbl.o1))
  else Cstruct.empty
