let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

open Lwt.Infix
open Git

let with_fd ~mode ~f device uid acc =
  Cstruct_append.create ~mode device uid >>= function
  | Ok fd ->
      f fd acc >>= fun acc ->
      Cstruct_append.close device fd >>= fun _ ->
      Gc.full_major ();
      Lwt.return acc
  | Error err -> Alcotest.failf "%a" Cstruct_append.pp_error err

let test_simple_use =
  Alcotest_lwt.test_case "simple use" `Quick @@ fun _sw () ->
  let device = Cstruct_append.device () in
  let a = Cstruct_append.key device in
  with_fd ~mode:Wr ~f:(Cstruct_append.append device) device a "Hello World!"
  >>= fun () ->
  Gc.full_major ();
  let v = Cstruct_append.project device a in
  Gc.full_major ();
  Alcotest.(check string) "contents" (Cstruct.to_string v) "Hello World!";
  Lwt.return_unit

(* XXX(dinosaure): any (re-)open of a filename/uid will be truncated
 * to length 0 (as [Unix.O_TRUNC]). *)
let test_trunk_mode =
  Alcotest_lwt.test_case "O_TRUNC" `Quick @@ fun _sw () ->
  let device = Cstruct_append.device () in
  let a = Cstruct_append.key device in
  with_fd ~mode:Wr ~f:(Cstruct_append.append device) device a "Hello World!"
  >>= fun () ->
  Gc.full_major ();
  with_fd ~mode:Rd
    ~f:(fun fd _ -> Cstruct_append.map device fd ~pos:0L 12 |> Lwt.return)
    device a Bigstringaf.empty
  >>= fun v ->
  Gc.full_major ();
  Alcotest.(check string) "contents" (Bigstringaf.to_string v) "";
  Lwt.return_unit

let test_two_contents =
  Alcotest_lwt.test_case "two contents" `Quick @@ fun _sw () ->
  let device = Cstruct_append.device () in
  let a = Cstruct_append.key device in
  let b = Cstruct_append.key device in
  with_fd ~mode:Wr ~f:(Cstruct_append.append device) device a "foo"
  >>= fun () ->
  Gc.full_major ();
  with_fd ~mode:Wr ~f:(Cstruct_append.append device) device b "bar"
  >>= fun () ->
  Gc.full_major ();
  let va = Cstruct_append.project device a in
  Gc.full_major ();
  let vb = Cstruct_append.project device b in
  Gc.full_major ();
  Alcotest.(check string) "contents" (Cstruct.to_string va) "foo";
  Alcotest.(check string) "contents" (Cstruct.to_string vb) "bar";
  Lwt.return_unit

let test_three_contents =
  Alcotest_lwt.test_case "three contents" `Quick @@ fun _sw () ->
  let device = Cstruct_append.device () in
  let a = Cstruct_append.key device in
  let b = Cstruct_append.key device in
  with_fd ~mode:Wr ~f:(Cstruct_append.append device) device a "foo"
  >>= fun () ->
  Gc.full_major ();
  with_fd ~mode:Wr ~f:(Cstruct_append.append device) device b "bar"
  >>= fun () ->
  Gc.full_major ();
  let c = Cstruct_append.key device in
  with_fd ~mode:RdWr
    ~f:(fun fd str ->
      let v = Cstruct_append.map device fd ~pos:0L 1 in
      Alcotest.(check string) "contents" (Bigstringaf.to_string v) "\x00";
      Cstruct_append.append device fd str)
    device c "lol"
  >>= fun () ->
  Gc.full_major ();
  let vb = Cstruct_append.project device b in
  Gc.full_major ();
  let vc = Cstruct_append.project device c in
  Gc.full_major ();
  Alcotest.(check string) "contents" (Cstruct.to_string vb) "bar";
  Alcotest.(check string) "contents" (Cstruct.to_string vc) "lol";
  Lwt.return_unit

let run =
  Alcotest_lwt.run "cstruct_append"
    [
      ( "cstruct_append",
        [
          test_simple_use;
          (* test_trunk_mode; *)
          test_two_contents;
          test_three_contents;
        ] );
    ]

let () = Lwt_main.run run
