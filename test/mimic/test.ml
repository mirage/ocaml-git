let () = Printexc.record_backtrace true

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
      let len = min (Cstruct.length cs) (Bytes.length o - off) in
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

let fiber =
  Alcotest_lwt.run "mimic"
    [ "mimic", [ test_input_string; test_output_string ] ]

let () = Lwt_main.run fiber
