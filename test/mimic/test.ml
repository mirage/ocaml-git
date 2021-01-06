let () = Printexc.record_backtrace true
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_level ~all:true (Some Logs.Debug)
let () = Logs.set_reporter (Logs_fmt.reporter ~dst:Fmt.stderr ())

module Memory_flow0 :
  Mimic.Mirage_protocol.S with type endpoint = string * bytes = struct
  type flow = {
    mutable i : string;
    o : bytes;
    mutable p : int;
    mutable c : bool;
  }

  type error = |
  type write_error = [ `Closed ]

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let pp_write_error ppf = function
    | `Closed -> Fmt.string ppf "!Connection closed by peer"

  let read ({ i; _ } as flow) =
    let len = min 0x1000 (String.length i) in
    if len = 0 then (
      flow.c <- true;
      Lwt.return_ok `Eof)
    else (
      flow.i <- String.sub i len (String.length i - len);
      Lwt.return_ok (`Data (Cstruct.of_string ~off:0 ~len i)))

  let write ({ o; p = off; c; _ } as flow) cs =
    if c then Lwt.return_error `Closed
    else
      let len = min (Cstruct.len cs) (Bytes.length o - off) in
      Cstruct.blit_to_bytes cs 0 o off len;
      if len = 0 then flow.c <- true;
      flow.p <- flow.p + len;
      Lwt.return_ok ()

  let writev flow css =
    let open Lwt.Infix in
    let rec go = function
      | [] -> Lwt.return_ok ()
      | x :: r -> (
          write flow x >>= function
          | Ok () -> go r
          | Error _ as err -> Lwt.return err)
    in
    go css

  let close flow =
    flow.c <- true;
    Lwt.return ()

  type endpoint = string * bytes

  let connect (str, buf) = Lwt.return_ok { i = str; o = buf; p = 0; c = false }
end

let edn0, memory0 = Mimic.register ~name:"memory0" (module Memory_flow0)

module Flow = Unixiz.Make (Mimic)

let error = Alcotest.testable Flow.pp_error ( = )

let recv =
  let pp ppf = function
    | `End_of_flow -> Fmt.string ppf "`End_of_flow"
    | `Input len -> Fmt.pf ppf "(`Input %d)" len
  in
  Alcotest.testable pp ( = )

let send = Alcotest.int

let test_input_string =
  Alcotest_lwt.test_case "input string" `Quick @@ fun _sw () ->
  let open Rresult in
  let open Lwt.Infix in
  let ctx = Mimic.add edn0 ("Hello World!", Bytes.empty) Mimic.empty in
  Mimic.resolve ctx >>= fun flow ->
  Alcotest.(check bool) "resolve" (R.is_ok flow) true;
  let flow = Flow.make (R.get_ok flow) in
  let buf0 = Cstruct.create 12 in
  let buf1 = Cstruct.create 12 in
  Flow.recv flow buf0 >>= fun res0 ->
  Flow.recv flow buf1 >>= fun res1 ->
  Flow.send flow (Cstruct.of_string "Hello World!") >>= fun res2 ->
  Alcotest.(check (result recv error)) "res0" res0 (Ok (`Input 12));
  Alcotest.(check string) "buf0" (Cstruct.to_string buf0) "Hello World!";
  Alcotest.(check (result recv error)) "res1" res1 (Ok `End_of_flow);
  Alcotest.(check (result send error))
    "res2" res2
    (Error (`Write_error `Closed));
  Lwt.return_unit

let test_output_string =
  Alcotest_lwt.test_case "output string" `Quick @@ fun _sw () ->
  let open Rresult in
  let open Lwt.Infix in
  let buf = Bytes.create 12 in
  let ctx = Mimic.add edn0 ("", buf) Mimic.empty in
  Mimic.resolve ctx >>= fun flow ->
  Alcotest.(check bool) "resolve" (R.is_ok flow) true;
  let flow = Flow.make (R.get_ok flow) in
  Flow.send flow (Cstruct.of_string "Hell") >>= fun res0 ->
  Flow.send flow (Cstruct.of_string "o Wo") >>= fun res1 ->
  Flow.send flow (Cstruct.of_string "rld!") >>= fun res2 ->
  Flow.send flow (Cstruct.of_string "?!?!") >>= fun res3 ->
  Flow.recv flow Cstruct.empty >>= fun res4 ->
  Alcotest.(check (result send error)) "res0" (Ok 4) res0;
  Alcotest.(check (result send error)) "res1" (Ok 4) res1;
  Alcotest.(check (result send error)) "res2" (Ok 4) res2;
  Alcotest.(check (result send error)) "res3" (Ok 4) res3;
  (* FIXME(dinosaure) *)
  Alcotest.(check (result recv error)) "res4" (Ok `End_of_flow) res4;
  Alcotest.(check string) "buf" (Bytes.to_string buf) "Hello World!";
  Lwt.return_unit

module Fake (Edn : sig
  type t
end) =
struct
  type error = |
  type write_error = [ `Closed ]

  let pp_error : error Fmt.t = fun _ -> function _ -> .

  let pp_write_error : write_error Fmt.t =
   fun ppf `Closed -> Fmt.string ppf "Connection closed by peer"

  type flow = Edn.t

  and endpoint = Edn.t

  let connect (edn : endpoint) = Lwt.return_ok edn
  let read _ = Lwt.return_ok (`Data Cstruct.empty)
  let write _ _ = Lwt.return_ok ()
  let close _ = Lwt.return_unit
  let writev _ _ = Lwt.return_ok ()
end

let edn_int, protocol_int =
  Mimic.register ~name:"int" (module Fake (struct type t = int end))

module Protocol_int = (val Mimic.repr protocol_int)

let edn_string, protocol_string =
  Mimic.register ~name:"string" (module Fake (struct type t = string end))

module Protocol_string = (val Mimic.repr protocol_string)

let edn_float, protocol_float =
  Mimic.register ~name:"float" (module Fake (struct type t = float end))

module Protocol_float = (val Mimic.repr protocol_float)

let flow :
    type edn flow. (edn, flow) Mimic.protocol -> Mimic.flow Alcotest.testable =
 fun protocol ->
  let module Repr = (val Mimic.repr protocol) in
  let equal a b = match a, b with Repr.T a, Repr.T b -> a = b | _ -> false in
  let pp ppf _ = Fmt.string ppf "flow" in
  Alcotest.testable pp equal

let mimic_error = Alcotest.testable Mimic.pp_error ( = )

let test_values =
  Alcotest_lwt.test_case "values" `Quick @@ fun _sw () ->
  let open Lwt.Infix in
  let ctx0 = Mimic.empty |> Mimic.add edn_int 42 in
  Mimic.resolve ctx0 >>= fun res0 ->
  Alcotest.(check (result (flow protocol_int) mimic_error))
    "res0" res0 (Ok (Protocol_int.T 42));
  let ctx1 = Mimic.empty |> Mimic.add edn_string "Hello World!" in
  Mimic.resolve ctx1 >>= fun res1 ->
  Alcotest.(check (result (flow protocol_string) mimic_error))
    "res1" res1 (Ok (Protocol_string.T "Hello World!"));
  let ctx2 = Mimic.empty |> Mimic.add edn_float 0.42 in
  Mimic.resolve ctx2 >>= fun res2 ->
  Alcotest.(check (result (flow protocol_float) mimic_error))
    "res2" res2 (Ok (Protocol_float.T 0.42));
  Lwt.return_unit

let test_functions =
  Alcotest_lwt.test_case "functions" `Quick @@ fun _sw () ->
  let open Lwt.Infix in
  let k a b = Lwt.return_some (a + b) in
  let ka = Mimic.make ~name:"a" and kb = Mimic.make ~name:"b" in
  let ctx = Mimic.(fold edn_int Fun.[ req ka; req kb ] ~k Mimic.empty) in
  let ctx = Mimic.add ka 2 ctx in
  let ctx = Mimic.add kb 3 ctx in
  Mimic.resolve ctx >>= fun res0 ->
  Alcotest.(check (result (flow protocol_int) mimic_error))
    "res0" res0 (Ok (Protocol_int.T 5));
  let kint = Mimic.make ~name:"int" in
  let k v = Lwt.return_some (string_of_int v) in
  let ctx0 = Mimic.(fold edn_string Fun.[ dft kint 42 ] ~k Mimic.empty) in
  let ctx1 = Mimic.add kint 51 ctx0 in
  Mimic.resolve ctx0 >>= fun res1 ->
  Alcotest.(check (result (flow protocol_string) mimic_error))
    "res1" res1 (Ok (Protocol_string.T "42"));
  Mimic.resolve ctx1 >>= fun res2 ->
  Alcotest.(check (result (flow protocol_string) mimic_error))
    "res2" res2 (Ok (Protocol_string.T "51"));
  Lwt.return_unit

let test_topological_sort =
  Alcotest_lwt.test_case "topologicial" `Quick @@ fun _sw () ->
  let open Lwt.Infix in
  let k v = Lwt.return_some (string_of_int v) in
  let kint01 = Mimic.make ~name:"int01" in
  let ctx = Mimic.empty in
  let ctx = Mimic.(fold edn_string Fun.[ req kint01 ] ~k ctx) in
  let kint02 = Mimic.make ~name:"int02" in
  let k v = Lwt.return_some (succ v) in
  let ctx = Mimic.(fold kint01 Fun.[ req kint02 ] ~k ctx) in
  let ctx0 = Mimic.add kint01 5 ctx in
  let ctx1 = Mimic.add kint02 4 ctx in
  Mimic.resolve ctx0 >>= fun res0 ->
  Alcotest.(check (result (flow protocol_string) mimic_error))
    "res0" res0 (Ok (Protocol_string.T "5"));
  Mimic.resolve ctx1 >>= fun res1 ->
  Alcotest.(check (result (flow protocol_string) mimic_error))
    "res1" res1 (Ok (Protocol_string.T "5"));
  Mimic.resolve ctx >>= fun res2 ->
  Alcotest.(check (result (flow protocol_string) mimic_error))
    "res2" res2
    (Error `Not_found);
  Alcotest.(check (result (flow protocol_int) mimic_error))
    "res2" res2
    (Error `Not_found);
  Lwt.return_unit

let test_priority =
  Alcotest_lwt.test_case "priority" `Quick @@ fun _sw () ->
  let open Lwt.Infix in
  let int_edn0, int_ptr0 =
    Mimic.register ~priority:10 ~name:"int0"
      (module Fake (struct type t = int end))
  in
  let int_edn1, int_ptr1 =
    Mimic.register ~priority:20 ~name:"int1"
      (module Fake (struct type t = int end))
  in
  let ctx0 = Mimic.empty |> Mimic.add int_edn0 1 |> Mimic.add int_edn1 2 in
  let ctx1 = Mimic.empty |> Mimic.add int_edn1 2 |> Mimic.add int_edn0 1 in
  Mimic.resolve ctx0 >>= fun res0 ->
  Mimic.resolve ctx1 >>= fun res1 ->
  let module Int0 = (val Mimic.repr int_ptr0) in
  Alcotest.(check (result (flow int_ptr0) mimic_error))
    "res0" res0 (Ok (Int0.T 1));
  Alcotest.(check (result (flow int_ptr0) mimic_error))
    "res1" res1 (Ok (Int0.T 1));
  let int_edn2, _ =
    Mimic.register ~name:"int2" (module Fake (struct type t = int end))
  in
  let ctx0 = Mimic.empty |> Mimic.add int_edn1 2 |> Mimic.add int_edn2 3 in
  let ctx1 =
    Mimic.empty
    |> Mimic.add int_edn2 3
    |> Mimic.add int_edn1 2
    |> Mimic.add int_edn0 1
  in
  Mimic.resolve ctx0 >>= fun res2 ->
  Mimic.resolve ctx1 >>= fun res3 ->
  let module Int1 = (val Mimic.repr int_ptr1) in
  Alcotest.(check (result (flow int_ptr1) mimic_error))
    "res2" res2 (Ok (Int1.T 2));
  Alcotest.(check (result (flow int_ptr0) mimic_error))
    "res3" res3 (Ok (Int0.T 1));
  let int_edn3, int_ptr3 =
    Mimic.register ~priority:20 ~name:"int3"
      (module Fake (struct type t = int end))
  in
  let ctx0 = Mimic.empty |> Mimic.add int_edn1 2 |> Mimic.add int_edn3 4 in
  let ctx1 = Mimic.empty |> Mimic.add int_edn3 4 |> Mimic.add int_edn1 2 in
  Mimic.resolve ctx0 >>= fun res4 ->
  Mimic.resolve ctx1 >>= fun res5 ->
  let module Int3 = (val Mimic.repr int_ptr3) in
  Alcotest.(check (result (flow int_ptr3) mimic_error))
    "res4" res4 (Ok (Int3.T 4));
  (* XXX(dinosaure): if two roots exist, we take the most recently registered!
     We should provide an other semantic like: the most recently inserted into the [ctx]. *)
  Alcotest.(check (result (flow int_ptr3) mimic_error))
    "res5" res5 (Ok (Int3.T 4));
  Lwt.return_unit

let fiber =
  Alcotest_lwt.run "mimic"
    [
      ( "mimic",
        [
          test_input_string; test_output_string; test_values; test_functions;
          test_topological_sort; test_priority;
        ] );
    ]

let () = Lwt_main.run fiber
