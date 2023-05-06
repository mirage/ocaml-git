open Bos
open Rresult
open Lwt_backend
open Store_backend
module Flow = Unixiz.Make (Mimic)

(** to keep track of directories created by unit tests and clean them up afterwards *)
module Tmp_dirs = struct
  let rm_r dir = OS.Dir.delete ~recurse:true dir |> ignore
  let t = ref Fpath.Set.empty
  let add file = t := Fpath.Set.add file !t

  let remove_all () =
    Fpath.Set.iter rm_r !t;
    t := Fpath.Set.empty

  let are_valid = ref true
end

let ( >>? ) x f =
  let open Lwt.Infix in
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

module Option = struct
  include Option

  let value_else o ~else_ = match o with Some v -> v | None -> else_ ()
end

let create_tmp_dir ?(mode = 0o700) ?prefix_path pat =
  let dir = Option.value_else prefix_path ~else_:OS.Dir.default_tmp in
  let failed_too_many_times () =
    R.error_msgf
      "create temporary directory %s in %a: too many failing attempts"
      (Fmt.str pat "XXXXXX") Fpath.pp dir
  in
  let rec loop count =
    if count < 0 then failed_too_many_times ()
    else
      let dir =
        let rand = Random.bits () land 0xffffff in
        Fpath.(dir / Fmt.str pat (Fmt.str "%06x" rand))
      in
      try
        Ok
          (Unix.mkdir (Fpath.to_string dir) mode;
           dir)
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
      | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
      | Unix.Unix_error (e, _, _) ->
          R.error_msgf "create temporary directory %s in %a: %s"
            (Fmt.str pat "XXXXXX") Fpath.pp dir (Unix.error_message e)
  in
  match loop 10000 with
  | Ok dir as r ->
      Tmp_dirs.add dir;
      r
  | Error _ as e -> e

let git_version =
  match
    Bos.(
      OS.Cmd.run_out Cmd.(v "git" % "--version") |> OS.Cmd.out_string ~trim:true)
  with
  | Error (`Msg err) -> failwith err
  | Ok (str, _) -> (
      match Git_version.parse str with
      | Some version -> version
      | None -> Fmt.failwith "Impossible to parse the Git version: %s" str)

let v2_28_0 =
  {
    Git_version.major = 2;
    minor = 28;
    patch = Some "0";
    revision = None;
    release_candidate = None;
  }

let git_init_with_branch branch =
  let open Bos in
  let open Rresult in
  if Git_version.compare git_version v2_28_0 < 0 then
    OS.Cmd.run Cmd.(v "git" % "init") >>= fun () ->
    OS.Cmd.run Cmd.(v "git" % "config" % "init.defaultBranch" % branch)
  else OS.Cmd.run Cmd.(v "git" % "init" % "-b" % branch)

let create_new_git_push_store _sw =
  let create () =
    create_tmp_dir "git-%s" >>= fun root ->
    OS.Dir.with_current root git_init_with_branch "master" |> R.join
    >>= fun () ->
    let access =
      Sigs.
        {
          get = get_object_for_packer lwt;
          parents = (fun _uid _store -> assert false);
          deref = deref lwt;
          locals = (fun _store -> assert false);
          shallowed = (fun _store -> assert false);
          shallow = (fun _store _uid -> assert false);
          unshallow = (fun _store _uid -> assert false);
        }
    in
    let light_load uid = lightly_load lwt root uid |> Scheduler.prj in
    let heavy_load uid = heavily_load lwt root uid |> Scheduler.prj in
    let store = store_inj { path = root; tbl = Hashtbl.create 0x100 } in
    R.ok ((access, light_load, heavy_load), store)
  in
  match create () with
  | Ok res -> Lwt.return res
  | Error err -> Fmt.failwith "%a" R.pp_msg err

module Git_sync =
  Smart_git.Make_server (Scheduler) (Flow) (Append) (Append) (Uid) (Ref)

let loopback_endpoint, loopback =
  Mimic.register ~name:"loopback" (module Loopback)

let ctx_with_payloads ?(transmission = `Git) payloads =
  Mimic.empty
  |> Mimic.add loopback_endpoint payloads
  |> Mimic.add Smart_git.git_transmission transmission

let flow_with_payloads ?(transmission = `Git) payloads =
  let ctx = ctx_with_payloads ~transmission payloads in
  Mimic.resolve ctx

let handle_error = function
  | Ok x -> Lwt.return x
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let test_cancelled_fetch () =
  Alcotest_lwt.test_case "cancelled fetch" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    (* let capabilities = [] in *)
    let payloads = [ "\x30\x30\x30\x30" (* 0000 *) ] in
    create_new_git_push_store sw
    >>= fun (access, store) ->
    flow_with_payloads (payloads, ignore) >>? fun flow ->
    Git_sync.upload_pack (Flow.make flow) access store >>= fun () ->
    Lwt.return (Ok ())
  in
  (* TODO: Test that the flow receive the expected response:
     - List of references (head, master) *)
  run () >>= handle_error

(* let test_fetch_all () = *)
(*   Alcotest_lwt.test_case "fetch all" `Quick @@ fun sw () -> *)
(*   let open Lwt.Infix in *)
(*   let run () = *)
(*     Lwt.return @@ Ok () *)
(*     (1* Client send 0009done *1) *)
(*     (1* Server should send: *)
(*        - List of refs *)
(*        - pack containing all commits *1) *)
(*   in *)
(*   run () >>= handle_error *)

let test =
  Alcotest_lwt.run "server" [ "upload-pack", [ test_cancelled_fetch () ] ]

let tmp = "tmp/server"

let () =
  let fiber =
    OS.Dir.current () >>= fun current ->
    OS.Dir.create Fpath.(current / tmp) >>= fun _ -> R.ok Fpath.(current / tmp)
  in
  let tmp = R.failwith_error_msg fiber in
  OS.Dir.set_default_tmp tmp;
  Lwt_main.run test
