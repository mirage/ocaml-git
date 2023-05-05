open Bos
open Rresult
open Lwt_backend
open Store_backend

(** logging: *)
let () = Printexc.record_backtrace true

let v2_28_0 =
  {
    Git_version.major = 2;
    minor = 28;
    patch = Some "0";
    revision = None;
    release_candidate = None;
  }

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

let git_init_with_branch branch =
  let open Bos in
  let open Rresult in
  if Git_version.compare git_version v2_28_0 < 0 then
    OS.Cmd.run Cmd.(v "git" % "init") >>= fun () ->
    OS.Cmd.run Cmd.(v "git" % "config" % "init.defaultBranch" % branch)
  else OS.Cmd.run Cmd.(v "git" % "init" % "-b" % branch)

let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      let path = Fpath.v (Unix.getcwd ()) in
      Format.kfprintf k ppf
        ("%a[%a][%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Blue string)
        (Fpath.basename path)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Mirage_crypto_rng_unix.initialize (module Mirage_crypto_rng.Fortuna)
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

(** utils:  *)
let ( >>? ) x f =
  let open Lwt.Infix in
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

module Option = struct
  include Option

  let value_else o ~else_ = match o with Some v -> v | None -> else_ ()
end

(** conduit-related setup for tests: *)

let pipe_value, pipe = Mimic.register ~name:"pipe" (module Pipe)

let ctx_with_pipe ?cwd ?(env = [||]) ?(args = [||]) cmd =
  Mimic.add pipe_value { cmd; args; env; cwd } Mimic.empty

let fifo_value, fifo = Mimic.register ~name:"fifo" (module Fifo)

let ctx_with_fifo ic oc =
  Mimic.empty
  |> Mimic.add fifo_value (ic, oc)
  |> Mimic.add Smart_git.git_transmission `Exec

let loopback_endpoint, loopback =
  Mimic.register ~name:"loopback" (module Loopback)

let ctx_with_payloads ?(transmission = `Git) payloads =
  Mimic.empty
  |> Mimic.add loopback_endpoint payloads
  |> Mimic.add Smart_git.git_transmission transmission

(** Alcotest setup for testing: *)
let uid = Alcotest.testable Uid.pp Uid.equal

let git_ref = Alcotest.testable Git.Reference.pp Git.Reference.equal

let ref_contents =
  Alcotest.testable
    (Git.Reference.pp_contents ~pp:Uid.pp)
    (Git.Reference.equal_contents ~equal:Uid.equal)

(** tmp dir management: *)

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

(* let () = at_exit (fun () -> if !Tmp_dirs.are_valid then Tmp_dirs.remove_all ()) *)

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

(* XXX(dinosaure): FIFO "à la BOS". *)

(** to keep track of named pipes (aka FIFOs) created by unit tests
    and clean them up afterwards *)
module Tmp_fifos = struct
  let rec unlink fifo =
    try Unix.unlink (Fpath.to_string fifo) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> unlink fifo
    | Unix.Unix_error _ -> ()

  let t = ref Fpath.Set.empty
  let add fifo = t := Fpath.Set.add fifo !t
  let unlink_all () = Fpath.Set.iter unlink !t
end

let () = at_exit Tmp_fifos.unlink_all

let create_fifo_path mode dir pat =
  let err () =
    R.error_msgf "create temporary fifo %s in %a: too many failing attempts"
      (Fmt.str pat "XXXXXX") Fpath.pp dir
  in
  let rec loop count =
    if count < 0 then err ()
    else
      let file =
        let rand = Random.bits () land 0xffffff in
        Fpath.(dir / Fmt.str pat (Fmt.str "%06x" rand))
      in
      let sfile = Fpath.to_string file in
      try
        Unix.mkfifo sfile mode;
        Ok file
      with
      | Unix.Unix_error (Unix.EEXIST, _, _) -> loop (count - 1)
      | Unix.Unix_error (Unix.EINTR, _, _) -> loop count
      | Unix.Unix_error (e, _, _) ->
          R.error_msgf "create temporary fifo %a: %s" Fpath.pp file
            (Unix.error_message e)
  in
  loop 10000

let with_fifo ?(mode = 0o600) ?dir pat =
  let dir = Option.value_else dir ~else_:OS.Dir.default_tmp in
  create_fifo_path mode dir pat >>| fun file ->
  Tmp_fifos.add file;
  file

let create_new_git_store _sw =
  let create () =
    (* XXX(dinosaure): a hook is already added by [Bos] to delete the
       directory. *)
    create_tmp_dir "git-%s" >>= fun root ->
    OS.Dir.with_current root git_init_with_branch "master" |> R.join
    >>= fun () ->
    let access = access lwt in
    let light_load uid = lightly_load lwt root uid |> Scheduler.prj in
    let heavy_load uid = heavily_load lwt root uid |> Scheduler.prj in
    let store = store_inj { path = root; tbl = Hashtbl.create 0x100 } in
    R.ok ((access, light_load, heavy_load), store)
  in
  match create () with
  | Ok res -> Lwt.return res
  | Error err -> Fmt.failwith "%a" R.pp_msg err

let empty_repository_fetch =
  [
    "\x30\x31\x33\x62\x33\x31\x61\x30\x37\x66\x30\x64\x64\x38\x61\x37"
    (* 013b31a07f0dd8a7 *);
    "\x66\x35\x31\x36\x37\x61\x65\x61\x63\x64\x36\x65\x32\x65\x66\x66"
    (* f5167aeacd6e2eff *);
    "\x33\x35\x66\x66\x33\x61\x37\x64\x62\x31\x64\x31\x20\x48\x45\x41"
    (* 35ff3a7db1d1 HEA *);
    "\x44\x00\x6d\x75\x6c\x74\x69\x5f\x61\x63\x6b\x20\x74\x68\x69\x6e"
    (* D.multi_ack thin *);
    "\x2d\x70\x61\x63\x6b\x20\x73\x69\x64\x65\x2d\x62\x61\x6e\x64\x20"
    (* -pack side-band  *);
    "\x73\x69\x64\x65\x2d\x62\x61\x6e\x64\x2d\x36\x34\x6b\x20\x6f\x66"
    (* side-band-64k of *);
    "\x73\x2d\x64\x65\x6c\x74\x61\x20\x73\x68\x61\x6c\x6c\x6f\x77\x20"
    (* s-delta shallow  *);
    "\x64\x65\x65\x70\x65\x6e\x2d\x73\x69\x6e\x63\x65\x20\x64\x65\x65"
    (* deepen-since dee *);
    "\x70\x65\x6e\x2d\x6e\x6f\x74\x20\x64\x65\x65\x70\x65\x6e\x2d\x72"
    (* pen-not deepen-r *);
    "\x65\x6c\x61\x74\x69\x76\x65\x20\x6e\x6f\x2d\x70\x72\x6f\x67\x72"
    (* elative no-progr *);
    "\x65\x73\x73\x20\x69\x6e\x63\x6c\x75\x64\x65\x2d\x74\x61\x67\x20"
    (* ess include-tag  *);
    "\x6d\x75\x6c\x74\x69\x5f\x61\x63\x6b\x5f\x64\x65\x74\x61\x69\x6c"
    (* multi_ack_detail *);
    "\x65\x64\x20\x61\x6c\x6c\x6f\x77\x2d\x74\x69\x70\x2d\x73\x68\x61"
    (* ed allow-tip-sha *);
    "\x31\x2d\x69\x6e\x2d\x77\x61\x6e\x74\x20\x61\x6c\x6c\x6f\x77\x2d"
    (* 1-in-want allow- *);
    "\x72\x65\x61\x63\x68\x61\x62\x6c\x65\x2d\x73\x68\x61\x31\x2d\x69"
    (* reachable-sha1-i *);
    "\x6e\x2d\x77\x61\x6e\x74\x20\x73\x79\x6d\x72\x65\x66\x3d\x48\x45"
    (* n-want symref=HE *);
    "\x41\x44\x3a\x72\x65\x66\x73\x2f\x68\x65\x61\x64\x73\x2f\x6d\x61"
    (* AD:refs/heads/ma *);
    "\x73\x74\x65\x72\x20\x66\x69\x6c\x74\x65\x72\x20\x61\x67\x65\x6e"
    (* ster filter agen *);
    "\x74\x3d\x67\x69\x74\x2f\x67\x69\x74\x68\x75\x62\x2d\x67\x37\x63"
    (* t=git/github-g7c *);
    "\x33\x37\x65\x63\x38\x65\x39\x65\x31\x38\x0a\x30\x30\x33\x66\x33"
    (* 37ec8e9e18.003f3 *);
    "\x31\x61\x30\x37\x66\x30\x64\x64\x38\x61\x37\x66\x35\x31\x36\x37"
    (* 1a07f0dd8a7f5167 *);
    "\x61\x65\x61\x63\x64\x36\x65\x32\x65\x66\x66\x33\x35\x66\x66\x33"
    (* aeacd6e2eff35ff3 *);
    "\x61\x37\x64\x62\x31\x64\x31\x20\x72\x65\x66\x73\x2f\x68\x65\x61"
    (* a7db1d1 refs/hea *);
    "\x64\x73\x2f\x6d\x61\x73\x74\x65\x72\x0a\x30\x30\x30\x30\x30\x30"
    (* ds/master.000000 *);
    "\x30\x38\x4e\x41\x4b\x0a\x30\x30\x32\x33\x02\x45\x6e\x75"
    (* 08NAK.0023.Enu *);
    "\x6d\x65\x72\x61\x74\x69\x6e\x67\x20\x6f\x62\x6a\x65\x63\x74\x73"
    (* merating objects *);
    "\x3a\x20\x32\x2c\x20\x64\x6f\x6e\x65\x2e\x0a\x30\x30\x36\x33\x02"
    (* : 2, done..0063. *);
    "\x43\x6f\x75\x6e\x74\x69\x6e\x67\x20\x6f\x62\x6a\x65\x63\x74\x73"
    (* Counting objects *);
    "\x3a\x20\x20\x35\x30\x25\x20\x28\x31\x2f\x32\x29\x0d\x43\x6f\x75"
    (* :  50% (1/2).Cou *);
    "\x6e\x74\x69\x6e\x67\x20\x6f\x62\x6a\x65\x63\x74\x73\x3a\x20\x31"
    (* nting objects: 1 *);
    "\x30\x30\x25\x20\x28\x32\x2f\x32\x29\x0d\x43\x6f\x75\x6e\x74\x69"
    (* 00% (2/2).Counti *);
    "\x6e\x67\x20\x6f\x62\x6a\x65\x63\x74\x73\x3a\x20\x31\x30\x30\x25"
    (* ng objects: 100% *);
    "\x20\x28\x32\x2f\x32\x29\x2c\x20\x64\x6f\x6e\x65\x2e\x0a\x30\x30"
    (*  (2/2), done..00 *);
    "\x61\x61\x01\x50\x41\x43\x4b\x00\x00\x00\x02\x00\x00\x00\x02\x9c"
    (* aa.PACK......... *);
    "\x0c\x78\x9c\xad\x8c\x41\x0a\xc2\x30\x10\x00\xef\x79\xc5\xde\x85"
    (* .x...A..0...y... *);
    "\xb0\x4d\x93\x90\x80\x88\xe0\x0f\xfc\xc1\x6e\xba\xd5\x40\x63\x20"
    (* .M........n..@c  *);
    "\x5d\xff\x6f\xf1\xe4\x03\x3c\x0d\xcc\xc0\xe8\x10\x01\xcf\xc9\x85"
    (* ].o...<......... *);
    "\xa5\x44\xef\x0a\x47\xe1\x4c\x18\x51\x82\xe7\x35\x2d\x31\xbb\x94"
    (* .D..G.L.Q..5-1.. *);
    "\x56\x16\xf1\x19\xbd\xa1\xb7\x3e\xfb\x80\x1b\x6d\xb4\x97\xca\xa2"
    (* V......>...m.... *);
    "\x4a\x70\xef\x8d\xea\x0b\xce\xe3\x4b\x5b\x7e\xda\xf5\x71\xa8\xcd"
    (* Jp......K[~..q.. *);
    "\x96\xde\x2e\x30\x85\x3c\x87\x30\xa7\x38\xc1\x09\x1d\xa2\x39\x6c"
    (* ...0.<.0.8....9l *);
    "\xab\xaa\xf2\xa7\x9d\xb1\xe6\x03\x73\x7e\x40\xd4\x20\x78\x9c\x03"
    (* ........s~@. x.. *);
    "\x00\x00\x00\x00\x01\xad\x96\x6f\xaa\xe6\x80\x59\xc4\xaa\x39\xa3"
    (* .......o...Y..9. *);
    "\x15\x81\xae\xe6\x30\xc5\x7f\x3b\x30\x30\x30\x36\x01\x8b\x30\x30"
    (* ....0..;0006..00 *);
    "\x33\x61\x02\x54\x6f\x74\x61\x6c\x20\x32\x20\x28\x64\x65\x6c\x74"
    (* 3a.Total 2 (delt *);
    "\x61\x20\x30\x29\x2c\x20\x72\x65\x75\x73\x65\x64\x20\x32\x20\x28"
    (* a 0), reused 2 ( *);
    "\x64\x65\x6c\x74\x61\x20\x30\x29\x2c\x20\x70\x61\x63\x6b\x2d\x72"
    (* delta 0), pack-r *);
    "\x65\x75\x73\x65\x64\x20\x30\x0a\x30\x30\x30\x30" (* eused 0.0000 *);
  ]

(** test Sync.fetch: *)

type ('a, 'b, 'c) fetch_error = [ `Store of 'a | `Sync of 'b | `Bad_input of 'c ]

let store_err r = R.reword_error (fun e -> `Store e) r
let sync_err r = R.reword_error (fun e -> `Sync e) r
let bad_input_err r = R.reword_error (fun e -> `Bad_input e) r

let test_sync_fetch () =
  Alcotest_lwt.test_case "set local ref" `Quick @@ fun _switch () ->
  let open Lwt.Infix in
  let module Sync = Git.Mem.Sync (Git.Mem.Store) in
  let capabilities = [ `Side_band_64k ] in
  let head = Git.Reference.v "HEAD" in
  let empty_branch = Git.Reference.v "refs/heads/empty" in
  let master_branch = Git.Reference.v "refs/heads/master" in
  let payloads = empty_repository_fetch in
  let ctx = ctx_with_payloads (payloads, ignore) in
  Git.Mem.Store.v (Fpath.v "/")
  >|= store_err
  >>? (fun store ->
        Git.Mem.Store.Ref.write store master_branch
          (Git.Reference.Uid (Digestif.SHA1.of_hex "1000"))
        >|= store_err
        >>? fun () ->
        Smart_git.Endpoint.of_string "git://localhost/not-found.git"
        |> bad_input_err
        |> Lwt.return
        >>? fun endpoint ->
        (* fetch HEAD and write it to refs/heads/master *)
        Sync.fetch ~ctx ~capabilities endpoint store
          (`Some [ head, empty_branch; head, master_branch ])
        >|= sync_err
        >>? function
        | None -> Alcotest.fail "should've fetched a ref"
        | Some (_pack_hash, [ (fetched_ref, fetched_obj_id) ]) ->
            Alcotest.check git_ref "request reference is fetched" fetched_ref
              head;
            let refs_to_overwrite = [ empty_branch; master_branch ] in
            Lwt_list.map_s (Git.Mem.Store.Ref.read store) refs_to_overwrite
            >>= fun pointed_obj_ids ->
            List.combine refs_to_overwrite pointed_obj_ids
            |> List.fold_left
                 (fun () -> function
                   | _, Ok pointed_obj_id ->
                       Alcotest.check ref_contents "desired ref overwritten"
                         pointed_obj_id (Git.Reference.Uid fetched_obj_id)
                   | ref_, Error _ ->
                       Alcotest.failf "desired ref %a is not overwritten"
                         Git.Reference.pp ref_)
                 ()
            |> Lwt.return_ok
        | Some _ -> Alcotest.fail "fetched more refs than requested")
  >|= R.reword_error (function
        | `Store err -> Alcotest.failf "%a" Git.Mem.Store.pp_error err
        | `Sync err -> Alcotest.failf "%a" Sync.pp_error err
        | `Bad_input err -> Alcotest.failf "%a" R.pp_msg err)
  >|= ignore

(** test Smart_git-related functionality: *)

(* XXX(dinosaure): [tmp] without systemic deletion of directories. *)

module Git_sync = Smart_git.Make (Scheduler) (Append) (Append) (Uid) (Ref)

(* TODO(dinosaure): we don't check what we sent, we should check that. *)

let test_empty_clone () =
  Alcotest_lwt.test_case "empty clone" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    let capabilities = [] in
    let payloads = [ "\x30\x30\x30\x30" (* 0000 *) ] in
    create_new_git_store sw >>= fun (access, store) ->
    let { path; _ } = store_prj store in
    let pack, index =
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    let ctx = ctx_with_payloads (payloads, ignore) in
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    Smart_git.Endpoint.of_string "git://localhost/not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Git_sync.fetch ~ctx ~capabilities access store endpoint
      (`Some [ Ref.v "HEAD" ])
      pack index ~src:tmp0 ~dst:tmp1 ~idx:tmp2
  in
  run () >>= function
  | Ok `Empty -> Lwt.return_unit
  | Ok (`Pack _) -> Alcotest.failf "Unexpected PACK file"
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let test_simple_clone () =
  Alcotest_lwt.test_case "simple clone" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    let capabilities = [ `Side_band_64k ] in
    let payloads = empty_repository_fetch in
    create_new_git_store sw >>= fun (access, store) ->
    let { path; _ } = store_prj store in
    let pack, index =
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    let ctx = ctx_with_payloads (payloads, ignore) in
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    Smart_git.Endpoint.of_string "git://localhost/not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Git_sync.fetch ~ctx ~capabilities access store endpoint `All pack index
      ~src:tmp0 ~dst:tmp1 ~idx:tmp2
  in
  run () >>= function
  | Ok (`Pack _) -> Lwt.return_unit
  | Ok `Empty -> Alcotest.failf "Unexpected empty fetch"
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let create_new_git_push_store _sw =
  let create () =
    (* XXX(dinosaure): a hook is already added by [Bos] to delete the
       directory. *)
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

let commit_foo store =
  let { path; _ } = store_prj store in
  let commit =
    OS.Dir.with_current path @@ fun () ->
    OS.Cmd.run Cmd.(v "git" % "config" % "user.name" % "test") >>= fun () ->
    OS.Cmd.run Cmd.(v "git" % "config" % "user.email" % "pseudo@pseudo.invalid")
    >>= fun () ->
    OS.File.write (Fpath.v "foo") "" >>= fun () ->
    OS.Cmd.run Cmd.(v "git" % "add" % "foo") >>= fun () ->
    OS.Cmd.run Cmd.(v "git" % "commit" % "-m" % ".") >>= fun () ->
    let out =
      OS.Cmd.run_out
        Cmd.(v "git" % "show" % "-s" % "--pretty=format:%H" % "HEAD")
    in
    OS.Cmd.out_lines ~trim:true out
  in
  match R.join (commit ()) with
  | Ok (head :: _, _) -> Lwt.return_ok head
  | Ok ([], _) -> Lwt.return_error (R.msgf "[commit_foo]")
  | Error err -> Lwt.return_error err

let test_simple_push () =
  Alcotest_lwt.test_case "simple push" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    let capabilities = [ `Side_band_64k ] in
    let payloads =
      [
        "\x30\x30\x38\x65\x38\x38\x31\x35\x66\x38\x62\x32\x35\x64\x30\x37"
        (* 008e8815f8b25d07 *);
        "\x35\x30\x65\x64\x31\x30\x64\x65\x64\x36\x34\x39\x36\x33\x39\x35"
        (* 50ed10ded6496395 *);
        "\x32\x38\x32\x62\x36\x30\x31\x35\x62\x30\x34\x31\x20\x72\x65\x66"
        (* 282b6015b041 ref *);
        "\x73\x2f\x68\x65\x61\x64\x73\x2f\x6d\x61\x73\x74\x65\x72\x00\x72"
        (* s/heads/master.r *);
        "\x65\x70\x6f\x72\x74\x2d\x73\x74\x61\x74\x75\x73\x20\x64\x65\x6c"
        (* eport-status del *);
        "\x65\x74\x65\x2d\x72\x65\x66\x73\x20\x73\x69\x64\x65\x2d\x62\x61"
        (* ete-refs side-ba *);
        "\x6e\x64\x2d\x36\x34\x6b\x20\x71\x75\x69\x65\x74\x20\x61\x74\x6f"
        (* nd-64k quiet ato *);
        "\x6d\x69\x63\x20\x6f\x66\x73\x2d\x64\x65\x6c\x74\x61\x20\x61\x67"
        (* mic ofs-delta ag *);
        "\x65\x6e\x74\x3d\x67\x69\x74\x2f\x32\x2e\x37\x2e\x34\x0a\x30\x30"
        (* ent=git/2.7.4.00 *);
        "\x30\x30\x30\x30\x33\x30\x01\x30\x30\x30\x65\x75\x6e\x70\x61\x63"
        (* 000030.000eunpac *);
        "\x6b\x20\x6f\x6b\x0a\x30\x30\x31\x39\x6f\x6b\x20\x72\x65\x66\x73"
        (* k ok.0019ok refs *);
        "\x2f\x68\x65\x61\x64\x73\x2f\x6d\x61\x73\x74\x65\x72\x0a\x30\x30"
        (* /heads/master.00 *);
        "\x30\x30\x30\x30\x30\x30" (* 000000 *);
      ]
    in
    create_new_git_push_store sw >>= fun (access, store) ->
    commit_foo store >>= fun _head ->
    let ctx = ctx_with_payloads (payloads, ignore) in
    Smart_git.Endpoint.of_string "git://localhost/not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Git_sync.push ~ctx ~capabilities access store endpoint
      [ `Update (Ref.v "refs/head/master", Ref.v "refs/head/master") ]
  in
  run () >>= function
  | Ok () -> Lwt.return_unit
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let test_push_error () =
  Alcotest_lwt.test_case "push error" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    let capabilities = [ `Side_band_64k ] in
    let payloads =
      [
        "\x30\x30\x36\x65\x45\x52\x52\x20\x0a\x20\x20\x59\x6f\x75\x20\x63"
        (* 006eERR .  You c *);
        "\x61\x6e\x27\x74\x20\x70\x75\x73\x68\x20\x74\x6f\x20\x67\x69\x74"
        (* an't push to git *);
        "\x3a\x2f\x2f\x67\x69\x74\x68\x75\x62\x2e\x63\x6f\x6d\x2f\x64\x69"
        (* ://github.com/di *);
        "\x6e\x6f\x73\x61\x75\x72\x65\x2f\x65\x6d\x70\x74\x79\x2e\x67\x69"
        (* nosaure/empty.gi *);
        "\x74\x0a\x20\x20\x55\x73\x65\x20\x68\x74\x74\x70\x73\x3a\x2f\x2f"
        (* t.  Use https:// *);
        "\x67\x69\x74\x68\x75\x62\x2e\x63\x6f\x6d\x2f\x64\x69\x6e\x6f\x73"
        (* github.com/dinos *);
        "\x61\x75\x72\x65\x2f\x65\x6d\x70\x74\x79\x2e\x67\x69\x74"
        (* aure/empty.git *);
      ]
    in
    create_new_git_push_store sw >>= fun (access, store) ->
    let ctx = ctx_with_payloads (payloads, ignore) in
    Smart_git.Endpoint.of_string "git://localhost/not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Git_sync.push ~ctx ~capabilities access store endpoint
      [ `Update (Ref.v "refs/head/master", Ref.v "refs/head/master") ]
  in
  run () >>= function
  | Ok () -> Alcotest.failf "Unexpected good result"
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (`Msg _) ->
      Alcotest.(check pass) "error" () ();
      Lwt.return_unit
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let test_fetch_empty () =
  Alcotest_lwt.test_case "fetch empty" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    let capabilities = [ `Side_band_64k ] in
    let payloads = empty_repository_fetch in
    create_new_git_store sw >>= fun (access, store) ->
    let { path; _ } = store_prj store in
    let pack, index =
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    let ctx = ctx_with_payloads (payloads, ignore) in
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    Smart_git.Endpoint.of_string "git://localhost/not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Git_sync.fetch ~ctx ~capabilities access store endpoint `All pack index
      ~src:tmp0 ~dst:tmp1 ~idx:tmp2
    >>? function
    | `Empty -> Alcotest.fail "Unexpected empty fetch"
    | `Pack (uid, refs) ->
        let { path; _ } = store_prj store in
        let dst =
          Fpath.(
            path
            / ".git"
            / "objects"
            / "pack"
            / Fmt.str "pack-%a.pack" Uid.pp uid)
        in
        OS.Path.move tmp1 dst |> Lwt.return >>? fun () ->
        let dst =
          Fpath.(
            path
            / ".git"
            / "objects"
            / "pack"
            / Fmt.str "pack-%a.idx" Uid.pp uid)
        in
        OS.Path.move tmp2 dst |> Lwt.return >>? fun () ->
        let update (refname, uid) =
          OS.Dir.with_current path @@ fun () ->
          OS.Cmd.run
            Cmd.(
              v "git" % "update-ref" % Ref.to_string refname % Uid.to_hex uid)
        in
        List.fold_right
          (fun v -> function Ok a -> R.join (update v a) | err -> err)
          refs (Ok ())
        |> Lwt.return
        >>? fun () ->
        let payloads =
          [
            "\x30\x31\x33\x62\x33\x31\x61\x30\x37\x66\x30\x64\x64\x38\x61\x37"
            (* 013b31a07f0dd8a7 *);
            "\x66\x35\x31\x36\x37\x61\x65\x61\x63\x64\x36\x65\x32\x65\x66\x66"
            (* f5167aeacd6e2eff *);
            "\x33\x35\x66\x66\x33\x61\x37\x64\x62\x31\x64\x31\x20\x48\x45\x41"
            (* 35ff3a7db1d1 HEA *);
            "\x44\x00\x6d\x75\x6c\x74\x69\x5f\x61\x63\x6b\x20\x74\x68\x69\x6e"
            (* D.multi_ack thin *);
            "\x2d\x70\x61\x63\x6b\x20\x73\x69\x64\x65\x2d\x62\x61\x6e\x64\x20"
            (* -pack side-band  *);
            "\x73\x69\x64\x65\x2d\x62\x61\x6e\x64\x2d\x36\x34\x6b\x20\x6f\x66"
            (* side-band-64k of *);
            "\x73\x2d\x64\x65\x6c\x74\x61\x20\x73\x68\x61\x6c\x6c\x6f\x77\x20"
            (* s-delta shallow  *);
            "\x64\x65\x65\x70\x65\x6e\x2d\x73\x69\x6e\x63\x65\x20\x64\x65\x65"
            (* deepen-since dee *);
            "\x70\x65\x6e\x2d\x6e\x6f\x74\x20\x64\x65\x65\x70\x65\x6e\x2d\x72"
            (* pen-not deepen-r *);
            "\x65\x6c\x61\x74\x69\x76\x65\x20\x6e\x6f\x2d\x70\x72\x6f\x67\x72"
            (* elative no-progr *);
            "\x65\x73\x73\x20\x69\x6e\x63\x6c\x75\x64\x65\x2d\x74\x61\x67\x20"
            (* ess include-tag  *);
            "\x6d\x75\x6c\x74\x69\x5f\x61\x63\x6b\x5f\x64\x65\x74\x61\x69\x6c"
            (* multi_ack_detail *);
            "\x65\x64\x20\x61\x6c\x6c\x6f\x77\x2d\x74\x69\x70\x2d\x73\x68\x61"
            (* ed allow-tip-sha *);
            "\x31\x2d\x69\x6e\x2d\x77\x61\x6e\x74\x20\x61\x6c\x6c\x6f\x77\x2d"
            (* 1-in-want allow- *);
            "\x72\x65\x61\x63\x68\x61\x62\x6c\x65\x2d\x73\x68\x61\x31\x2d\x69"
            (* reachable-sha1-i *);
            "\x6e\x2d\x77\x61\x6e\x74\x20\x73\x79\x6d\x72\x65\x66\x3d\x48\x45"
            (* n-want symref=HE *);
            "\x41\x44\x3a\x72\x65\x66\x73\x2f\x68\x65\x61\x64\x73\x2f\x6d\x61"
            (* AD:refs/heads/ma *);
            "\x73\x74\x65\x72\x20\x66\x69\x6c\x74\x65\x72\x20\x61\x67\x65\x6e"
            (* ster filter agen *);
            "\x74\x3d\x67\x69\x74\x2f\x67\x69\x74\x68\x75\x62\x2d\x67\x37\x63"
            (* t=git/github-g7c *);
            "\x33\x37\x65\x63\x38\x65\x39\x65\x31\x38\x0a\x30\x30\x33\x66\x33"
            (* 37ec8e9e18.003f3 *);
            "\x31\x61\x30\x37\x66\x30\x64\x64\x38\x61\x37\x66\x35\x31\x36\x37"
            (* 1a07f0dd8a7f5167 *);
            "\x61\x65\x61\x63\x64\x36\x65\x32\x65\x66\x66\x33\x35\x66\x66\x33"
            (* aeacd6e2eff35ff3 *);
            "\x61\x37\x64\x62\x31\x64\x31\x20\x72\x65\x66\x73\x2f\x68\x65\x61"
            (* a7db1d1 refs/hea *);
            "\x64\x73\x2f\x6d\x61\x73\x74\x65\x72\x0a\x30\x30\x30\x30"
            (* ds/master.0000 *);
          ]
        in
        let ctx = ctx_with_payloads (payloads, ignore) in
        OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
        OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
        OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
        Smart_git.Endpoint.of_string "git://localhost/not-found.git"
        |> Lwt.return
        >>? fun endpoint ->
        Git_sync.fetch ~ctx ~capabilities access store endpoint `All pack index
          ~src:tmp0 ~dst:tmp1 ~idx:tmp2
  in
  run () >>= function
  | Ok `Empty -> Lwt.return_unit
  | Ok (`Pack _) -> Alcotest.failf "Unexpected PACK file"
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let update_testzone_0 store =
  let { path; _ } = store_prj store in
  let update =
    OS.Dir.with_current path @@ fun () ->
    OS.Cmd.run
      Cmd.(
        v "git"
        % "update-ref"
        % "refs/heads/master"
        % "f08d64523257528980115942481d5ddd13d2c1ba")
  in
  match R.join (update ()) with
  | Ok () -> Lwt.return_ok ()
  | Error err -> Lwt.return_error err

let test_negotiation () =
  Alcotest_lwt.test_case "negotiation" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    let capabilities =
      [ `Side_band_64k; `Multi_ack_detailed; `Thin_pack; `Ofs_delta ]
    in
    let payloads =
      [
        "\x30\x31\x33\x62\x62\x38\x38\x35\x39\x39\x63\x62\x34\x32\x31\x37"
        (* 013bb88599cb4217 *);
        "\x63\x31\x37\x35\x31\x31\x30\x66\x36\x65\x32\x61\x38\x31\x30\x30"
        (* c175110f6e2a8100 *);
        "\x37\x39\x64\x39\x35\x34\x35\x32\x34\x38\x31\x34\x20\x48\x45\x41"
        (* 79d954524814 HEA *);
        "\x44\x00\x6d\x75\x6c\x74\x69\x5f\x61\x63\x6b\x20\x74\x68\x69\x6e"
        (* D.multi_ack thin *);
        "\x2d\x70\x61\x63\x6b\x20\x73\x69\x64\x65\x2d\x62\x61\x6e\x64\x20"
        (* -pack side-band  *);
        "\x73\x69\x64\x65\x2d\x62\x61\x6e\x64\x2d\x36\x34\x6b\x20\x6f\x66"
        (* side-band-64k of *);
        "\x73\x2d\x64\x65\x6c\x74\x61\x20\x73\x68\x61\x6c\x6c\x6f\x77\x20"
        (* s-delta shallow  *);
        "\x64\x65\x65\x70\x65\x6e\x2d\x73\x69\x6e\x63\x65\x20\x64\x65\x65"
        (* deepen-since dee *);
        "\x70\x65\x6e\x2d\x6e\x6f\x74\x20\x64\x65\x65\x70\x65\x6e\x2d\x72"
        (* pen-not deepen-r *);
        "\x65\x6c\x61\x74\x69\x76\x65\x20\x6e\x6f\x2d\x70\x72\x6f\x67\x72"
        (* elative no-progr *);
        "\x65\x73\x73\x20\x69\x6e\x63\x6c\x75\x64\x65\x2d\x74\x61\x67\x20"
        (* ess include-tag  *);
        "\x6d\x75\x6c\x74\x69\x5f\x61\x63\x6b\x5f\x64\x65\x74\x61\x69\x6c"
        (* multi_ack_detail *);
        "\x65\x64\x20\x61\x6c\x6c\x6f\x77\x2d\x74\x69\x70\x2d\x73\x68\x61"
        (* ed allow-tip-sha *);
        "\x31\x2d\x69\x6e\x2d\x77\x61\x6e\x74\x20\x61\x6c\x6c\x6f\x77\x2d"
        (* 1-in-want allow- *);
        "\x72\x65\x61\x63\x68\x61\x62\x6c\x65\x2d\x73\x68\x61\x31\x2d\x69"
        (* reachable-sha1-i *);
        "\x6e\x2d\x77\x61\x6e\x74\x20\x73\x79\x6d\x72\x65\x66\x3d\x48\x45"
        (* n-want symref=HE *);
        "\x41\x44\x3a\x72\x65\x66\x73\x2f\x68\x65\x61\x64\x73\x2f\x6d\x61"
        (* AD:refs/heads/ma *);
        "\x73\x74\x65\x72\x20\x66\x69\x6c\x74\x65\x72\x20\x61\x67\x65\x6e"
        (* ster filter agen *);
        "\x74\x3d\x67\x69\x74\x2f\x67\x69\x74\x68\x75\x62\x2d\x67\x37\x63"
        (* t=git/github-g7c *);
        "\x33\x37\x65\x63\x38\x65\x39\x65\x31\x38\x0a\x30\x30\x33\x66\x62"
        (* 37ec8e9e18.003fb *);
        "\x38\x38\x35\x39\x39\x63\x62\x34\x32\x31\x37\x63\x31\x37\x35\x31"
        (* 88599cb4217c1751 *);
        "\x31\x30\x66\x36\x65\x32\x61\x38\x31\x30\x30\x37\x39\x64\x39\x35"
        (* 10f6e2a810079d95 *);
        "\x34\x35\x32\x34\x38\x31\x34\x20\x72\x65\x66\x73\x2f\x68\x65\x61"
        (* 4524814 refs/hea *);
        "\x64\x73\x2f\x6d\x61\x73\x74\x65\x72\x0a\x30\x30\x30\x30\x30\x30"
        (* ds/master.000000 *);
        "\x33\x38\x41\x43\x4b\x20\x66\x30\x38\x64\x36\x34\x35\x32\x33\x32"
        (* 38ACK f08d645232 *);
        "\x35\x37\x35\x32\x38\x39\x38\x30\x31\x31\x35\x39\x34\x32\x34\x38"
        (* 5752898011594248 *);
        "\x31\x64\x35\x64\x64\x64\x31\x33\x64\x32\x63\x31\x62\x61\x20\x63"
        (* 1d5ddd13d2c1ba c *);
        "\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x62\x33"
        (* ommon.0038ACK b3 *);
        "\x31\x33\x61\x33\x35\x39\x35\x35\x62\x36\x30\x66\x32\x62\x66\x33"
        (* 13a35955b60f2bf3 *);
        "\x36\x62\x30\x64\x32\x39\x34\x61\x30\x66\x33\x63\x31\x34\x39\x31"
        (* 6b0d294a0f3c1491 *);
        "\x64\x36\x66\x64\x35\x38\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30"
        (* d6fd58 common.00 *);
        "\x33\x38\x41\x43\x4b\x20\x31\x32\x64\x39\x64\x66\x66\x31\x61\x31"
        (* 38ACK 12d9dff1a1 *);
        "\x38\x65\x34\x34\x38\x31\x31\x62\x33\x38\x64\x61\x38\x62\x36\x30"
        (* 8e44811b38da8b60 *);
        "\x61\x62\x64\x61\x62\x62\x66\x66\x37\x32\x35\x35\x34\x35\x20\x63"
        (* abdabbff725545 c *);
        "\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x38\x33"
        (* ommon.0038ACK 83 *);
        "\x66\x33\x65\x65\x36\x63\x35\x38\x63\x64\x37\x30\x62\x61\x63\x37"
        (* f3ee6c58cd70bac7 *);
        "\x64\x36\x65\x65\x30\x64\x39\x37\x37\x61\x34\x30\x63\x64\x30\x34"
        (* d6ee0d977a40cd04 *);
        "\x65\x31\x34\x65\x39\x31\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30"
        (* e14e91 common.00 *);
        "\x33\x38\x41\x43\x4b\x20\x32\x33\x33\x62\x32\x61\x34\x33\x64\x63"
        (* 38ACK 233b2a43dc *);
        "\x34\x64\x36\x66\x31\x36\x31\x61\x36\x62\x32\x30\x37\x62\x35\x66"
        (* 4d6f161a6b207b5f *);
        "\x62\x65\x65\x66\x39\x39\x65\x64\x65\x65\x63\x63\x65\x36\x20\x63"
        (* beef99edeecce6 c *);
        "\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x64\x32"
        (* ommon.0038ACK d2 *);
        "\x36\x34\x36\x36\x38\x38\x37\x66\x33\x66\x61\x62\x36\x65\x39\x39"
        (* 6466887f3fab6e99 *);
        "\x35\x63\x35\x38\x61\x30\x63\x32\x61\x35\x64\x61\x33\x34\x62\x30"
        (* 5c58a0c2a5da34b0 *);
        "\x34\x61\x30\x64\x31\x38\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30"
        (* 4a0d18 common.00 *);
        "\x33\x38\x41\x43\x4b\x20\x31\x33\x38\x30\x61\x30\x62\x33\x35\x36"
        (* 38ACK 1380a0b356 *);
        "\x63\x31\x64\x65\x65\x36\x63\x31\x65\x31\x32\x30\x38\x35\x61\x32"
        (* c1dee6c1e12085a2 *);
        "\x30\x38\x33\x37\x33\x39\x31\x61\x66\x38\x34\x64\x32\x63\x20\x63"
        (* 0837391af84d2c c *);
        "\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x61\x39"
        (* ommon.0038ACK a9 *);
        "\x64\x30\x30\x35\x37\x65\x37\x36\x36\x63\x30\x61\x64\x33\x37\x31"
        (* d0057e766c0ad371 *);
        "\x38\x30\x31\x30\x37\x62\x32\x66\x36\x66\x30\x36\x62\x33\x30\x34"
        (* 80107b2f6f06b304 *);
        "\x34\x38\x61\x33\x66\x33\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30"
        (* 48a3f3 common.00 *);
        "\x33\x38\x41\x43\x4b\x20\x35\x62\x32\x64\x31\x34\x34\x37\x37\x64"
        (* 38ACK 5b2d14477d *);
        "\x31\x66\x36\x37\x66\x31\x61\x38\x62\x61\x32\x32\x39\x33\x30\x62"
        (* 1f67f1a8ba22930b *);
        "\x35\x36\x62\x32\x30\x30\x39\x36\x33\x36\x30\x33\x37\x31\x20\x63"
        (* 56b20096360371 c *);
        "\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x35\x38"
        (* ommon.0038ACK 58 *);
        "\x38\x35\x35\x37\x62\x38\x31\x31\x33\x61\x63\x63\x32\x36\x35\x66"
        (* 8557b8113acc265f *);
        "\x30\x34\x33\x66\x61\x31\x30\x62\x65\x35\x61\x35\x30\x33\x65\x33"
        (* 043fa10be5a503e3 *);
        "\x61\x34\x61\x32\x36\x34\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30"
        (* a4a264 common.00 *);
        "\x33\x38\x41\x43\x4b\x20\x36\x61\x65\x61\x61\x38\x39\x34\x36\x30"
        (* 38ACK 6aeaa89460 *);
        "\x64\x64\x66\x31\x65\x63\x35\x65\x64\x36\x34\x30\x65\x64\x39\x31"
        (* ddf1ec5ed640ed91 *);
        "\x64\x32\x37\x64\x35\x63\x39\x32\x35\x63\x38\x63\x62\x32\x20\x63"
        (* d27d5c925c8cb2 c *);
        "\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x34\x33"
        (* ommon.0038ACK 43 *);
        "\x65\x65\x31\x37\x64\x62\x32\x62\x34\x62\x38\x37\x32\x62\x63\x32"
        (* ee17db2b4b872bc2 *);
        "\x65\x65\x35\x30\x31\x33\x37\x65\x37\x37\x36\x33\x36\x30\x37\x61"
        (* ee50137e7763607a *);
        "\x33\x37\x66\x65\x64\x61\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30"
        (* 37feda common.00 *);
        "\x33\x38\x41\x43\x4b\x20\x39\x39\x61\x35\x65\x65\x38\x61\x32\x66"
        (* 38ACK 99a5ee8a2f *);
        "\x34\x37\x62\x63\x33\x36\x38\x35\x31\x64\x32\x36\x39\x66\x65\x37"
        (* 47bc36851d269fe7 *);
        "\x64\x62\x64\x37\x38\x38\x35\x37\x34\x30\x39\x34\x31\x64\x20\x63"
        (* dbd7885740941d c *);
        "\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x65\x64"
        (* ommon.0038ACK ed *);
        "\x66\x35\x64\x62\x31\x39\x36\x63\x34\x30\x63\x35\x35\x31\x38\x38"
        (* f5db196c40c55188 *);
        "\x34\x32\x32\x63\x64\x61\x30\x36\x31\x66\x39\x34\x35\x39\x61\x39"
        (* 422cda061f9459a9 *);
        "\x35\x61\x66\x32\x31\x63\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30"
        (* 5af21c common.00 *);
        "\x33\x38\x41\x43\x4b\x20\x66\x31\x65\x61\x31\x63\x64\x63\x39\x63"
        (* 38ACK f1ea1cdc9c *);
        "\x63\x33\x64\x38\x38\x31\x36\x32\x34\x39\x63\x66\x39\x64\x30\x61"
        (* c3d8816249cf9d0a *);
        "\x34\x39\x63\x63\x33\x37\x35\x33\x31\x30\x33\x37\x30\x39\x20\x63"
        (* 49cc3753103709 c *);
        "\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x35\x65"
        (* ommon.0038ACK 5e *);
        "\x35\x37\x62\x36\x64\x35\x65\x66\x31\x38\x63\x35\x32\x31\x66\x34"
        (* 57b6d5ef18c521f4 *);
        "\x39\x64\x34\x39\x31\x31\x35\x39\x63\x31\x34\x66\x34\x61\x30\x35"
        (* 9d491159c14f4a05 *);
        "\x35\x39\x34\x39\x63\x35\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30"
        (* 5949c5 common.00 *);
        "\x33\x37\x41\x43\x4b\x20\x35\x65\x35\x37\x62\x36\x64\x35\x65\x66"
        (* 37ACK 5e57b6d5ef *);
        "\x31\x38\x63\x35\x32\x31\x66\x34\x39\x64\x34\x39\x31\x31\x35\x39"
        (* 18c521f49d491159 *);
        "\x63\x31\x34\x66\x34\x61\x30\x35\x35\x39\x34\x39\x63\x35\x20\x72"
        (* c14f4a055949c5 r *);
        "\x65\x61\x64\x79\x0a\x30\x30\x30\x38\x4e\x41\x4b\x0a\x30\x30\x33"
        (* eady.0008NAK.003 *);
        "\x38\x41\x43\x4b\x20\x39\x64\x62\x35\x31\x61\x32\x65\x62\x34\x66"
        (* 8ACK 9db51a2eb4f *);
        "\x63\x36\x31\x34\x38\x36\x39\x34\x61\x35\x33\x64\x31\x36\x38\x65"
        (* c6148694a53d168e *);
        "\x36\x30\x38\x66\x66\x64\x65\x30\x37\x31\x35\x66\x65\x20\x63\x6f"
        (* 608ffde0715fe co *);
        "\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x66\x65\x61"
        (* mmon.0038ACK fea *);
        "\x34\x35\x30\x64\x38\x63\x63\x37\x66\x34\x66\x63\x38\x38\x33\x65"
        (* 450d8cc7f4fc883e *);
        "\x65\x66\x31\x35\x33\x34\x33\x61\x36\x38\x30\x33\x33\x61\x64\x35"
        (* ef15343a68033ad5 *);
        "\x63\x32\x38\x30\x32\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33"
        (* c2802 common.003 *);
        "\x38\x41\x43\x4b\x20\x65\x38\x38\x39\x35\x63\x37\x36\x63\x31\x63"
        (* 8ACK e8895c76c1c *);
        "\x37\x63\x36\x31\x62\x33\x30\x31\x62\x36\x32\x36\x32\x66\x63\x30"
        (* 7c61b301b6262fc0 *);
        "\x31\x32\x34\x32\x35\x32\x65\x35\x33\x34\x39\x38\x31\x20\x63\x6f"
        (* 124252e534981 co *);
        "\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x62\x64\x63"
        (* mmon.0038ACK bdc *);
        "\x30\x34\x66\x30\x61\x38\x61\x32\x65\x36\x37\x30\x39\x36\x64\x38"
        (* 04f0a8a2e67096d8 *);
        "\x39\x32\x31\x37\x39\x31\x62\x33\x61\x33\x65\x31\x64\x33\x65\x39"
        (* 921791b3a3e1d3e9 *);
        "\x30\x30\x37\x30\x38\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33"
        (* 00708 common.003 *);
        "\x38\x41\x43\x4b\x20\x37\x33\x61\x33\x64\x65\x30\x36\x63\x36\x66"
        (* 8ACK 73a3de06c6f *);
        "\x39\x32\x65\x31\x37\x63\x35\x63\x37\x32\x66\x64\x38\x36\x38\x36"
        (* 92e17c5c72fd8686 *);
        "\x38\x37\x32\x31\x33\x31\x33\x62\x32\x62\x62\x33\x32\x20\x63\x6f"
        (* 8721313b2bb32 co *);
        "\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x30\x38\x38"
        (* mmon.0038ACK 088 *);
        "\x61\x36\x38\x34\x36\x36\x38\x35\x63\x36\x66\x63\x66\x62\x32\x64"
        (* a6846685c6fcfb2d *);
        "\x62\x39\x31\x30\x61\x35\x36\x37\x37\x32\x34\x38\x30\x37\x62\x38"
        (* b910a567724807b8 *);
        "\x39\x66\x30\x37\x64\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33"
        (* 9f07d common.003 *);
        "\x38\x41\x43\x4b\x20\x66\x63\x32\x65\x62\x31\x30\x35\x37\x66\x33"
        (* 8ACK fc2eb1057f3 *);
        "\x39\x30\x34\x33\x39\x37\x36\x37\x32\x38\x39\x35\x34\x38\x30\x34"
        (* 9043976728954804 *);
        "\x31\x66\x36\x33\x64\x31\x37\x33\x35\x62\x34\x31\x34\x20\x63\x6f"
        (* 1f63d1735b414 co *);
        "\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x38\x66\x33"
        (* mmon.0038ACK 8f3 *);
        "\x36\x31\x39\x34\x39\x38\x31\x39\x35\x62\x37\x37\x61\x37\x61\x66"
        (* 619498195b77a7af *);
        "\x33\x61\x31\x66\x34\x62\x36\x30\x39\x34\x39\x36\x61\x63\x61\x63"
        (* 3a1f4b609496acac *);
        "\x31\x33\x65\x33\x36\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33"
        (* 13e36 common.003 *);
        "\x38\x41\x43\x4b\x20\x65\x35\x63\x65\x63\x63\x35\x31\x62\x63\x62"
        (* 8ACK e5cecc51bcb *);
        "\x33\x65\x62\x38\x62\x38\x30\x37\x62\x32\x61\x38\x30\x37\x31\x66"
        (* 3eb8b807b2a8071f *);
        "\x37\x30\x63\x65\x31\x34\x33\x31\x37\x61\x37\x62\x32\x20\x63\x6f"
        (* 70ce14317a7b2 co *);
        "\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x38\x37\x65"
        (* mmon.0038ACK 87e *);
        "\x64\x37\x38\x31\x64\x33\x32\x30\x64\x35\x65\x38\x66\x32\x65\x64"
        (* d781d320d5e8f2ed *);
        "\x35\x36\x34\x36\x65\x66\x34\x36\x66\x66\x34\x31\x37\x33\x61\x31"
        (* 5646ef46ff4173a1 *);
        "\x38\x65\x31\x63\x65\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33"
        (* 8e1ce common.003 *);
        "\x38\x41\x43\x4b\x20\x63\x65\x32\x61\x33\x61\x62\x31\x32\x61\x65"
        (* 8ACK ce2a3ab12ae *);
        "\x63\x66\x64\x30\x38\x35\x65\x37\x37\x62\x33\x38\x66\x38\x37\x38"
        (* cfd085e77b38f878 *);
        "\x63\x36\x63\x37\x33\x34\x38\x61\x66\x33\x63\x36\x30\x20\x63\x6f"
        (* c6c7348af3c60 co *);
        "\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x31\x31\x39"
        (* mmon.0038ACK 119 *);
        "\x63\x39\x34\x63\x61\x63\x64\x39\x66\x33\x63\x38\x32\x37\x65\x38"
        (* c94cacd9f3c827e8 *);
        "\x66\x66\x33\x30\x30\x38\x64\x33\x31\x39\x63\x32\x38\x33\x32\x66"
        (* ff3008d319c2832f *);
        "\x38\x34\x32\x37\x33\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33"
        (* 84273 common.003 *);
        "\x38\x41\x43\x4b\x20\x64\x32\x65\x39\x37\x36\x31\x64\x38\x61\x63"
        (* 8ACK d2e9761d8ac *);
        "\x64\x64\x35\x39\x35\x34\x62\x32\x61\x63\x38\x62\x35\x39\x35\x61"
        (* dd5954b2ac8b595a *);
        "\x61\x33\x34\x64\x66\x34\x36\x35\x64\x62\x64\x62\x32\x20\x63\x6f"
        (* a34df465dbdb2 co *);
        "\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x30\x38\x61"
        (* mmon.0038ACK 08a *);
        "\x37\x30\x63\x33\x65\x37\x36\x62\x30\x34\x39\x62\x62\x33\x37\x39"
        (* 70c3e76b049bb379 *);
        "\x66\x35\x38\x33\x36\x64\x37\x30\x31\x63\x39\x38\x65\x66\x36\x34"
        (* f5836d701c98ef64 *);
        "\x64\x37\x62\x37\x61\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33"
        (* d7b7a common.003 *);
        "\x38\x41\x43\x4b\x20\x36\x31\x62\x32\x37\x34\x64\x37\x36\x34\x35"
        (* 8ACK 61b274d7645 *);
        "\x35\x63\x64\x31\x34\x33\x30\x62\x65\x61\x63\x64\x34\x35\x33\x32"
        (* 5cd1430beacd4532 *);
        "\x33\x63\x32\x64\x36\x61\x66\x65\x34\x30\x38\x61\x30\x20\x63\x6f"
        (* 3c2d6afe408a0 co *);
        "\x6d\x6d\x6f\x6e\x0a\x30\x30\x33\x38\x41\x43\x4b\x20\x64\x61\x39"
        (* mmon.0038ACK da9 *);
        "\x62\x30\x31\x35\x37\x66\x34\x66\x36\x33\x32\x64\x32\x66\x61\x61"
        (* b0157f4f632d2faa *);
        "\x39\x31\x39\x35\x63\x35\x30\x37\x62\x37\x37\x64\x35\x34\x38\x38"
        (* 9195c507b77d5488 *);
        "\x64\x34\x39\x61\x66\x20\x63\x6f\x6d\x6d\x6f\x6e\x0a\x30\x30\x33"
        (* d49af common.003 *);
        "\x37\x41\x43\x4b\x20\x64\x61\x39\x62\x30\x31\x35\x37\x66\x34\x66"
        (* 7ACK da9b0157f4f *);
        "\x36\x33\x32\x64\x32\x66\x61\x61\x39\x31\x39\x35\x63\x35\x30\x37"
        (* 632d2faa9195c507 *);
        "\x62\x37\x37\x64\x35\x34\x38\x38\x64\x34\x39\x61\x66\x20\x72\x65"
        (* b77d5488d49af re *);
        "\x61\x64\x79\x0a\x30\x30\x30\x38\x4e\x41\x4b\x0a\x30\x30\x33\x31"
        (* ady.0008NAK.0031 *);
        "\x41\x43\x4b\x20\x64\x61\x39\x62\x30\x31\x35\x37\x66\x34\x66\x36"
        (* ACK da9b0157f4f6 *);
        "\x33\x32\x64\x32\x66\x61\x61\x39\x31\x39\x35\x63\x35\x30\x37\x62"
        (* 32d2faa9195c507b *);
        "\x37\x37\x64\x35\x34\x38\x38\x64\x34\x39\x61\x66\x0a\x30\x30\x32"
        (* 77d5488d49af.002 *);
        "\x33\x02\x45\x6e\x75\x6d\x65\x72\x61\x74\x69\x6e\x67\x20\x6f\x62"
        (* 3.Enumerating ob *);
        "\x6a\x65\x63\x74\x73\x3a\x20\x35\x2c\x20\x64\x6f\x6e\x65\x2e\x0a"
        (* jects: 5, done.. *);
        "\x30\x30\x32\x32\x02\x43\x6f\x75\x6e\x74\x69\x6e\x67\x20\x6f\x62"
        (* 0022.Counting ob *);
        "\x6a\x65\x63\x74\x73\x3a\x20\x20\x32\x30\x25\x20\x28\x31\x2f\x35"
        (* jects:  20% (1/5 *);
        "\x29\x0d\x30\x30\x33\x66\x02\x43\x6f\x75\x6e\x74\x69\x6e\x67\x20"
        (* ).003f.Counting  *);
        "\x6f\x62\x6a\x65\x63\x74\x73\x3a\x20\x20\x34\x30\x25\x20\x28\x32"
        (* objects:  40% (2 *);
        "\x2f\x35\x29\x0d\x43\x6f\x75\x6e\x74\x69\x6e\x67\x20\x6f\x62\x6a"
        (* /5).Counting obj *);
        "\x65\x63\x74\x73\x3a\x20\x20\x36\x30\x25\x20\x28\x33\x2f\x35\x29"
        (* ects:  60% (3/5) *);
        "\x0d\x30\x30\x33\x66\x02\x43\x6f\x75\x6e\x74\x69\x6e\x67\x20\x6f"
        (* .003f.Counting o *);
        "\x62\x6a\x65\x63\x74\x73\x3a\x20\x20\x38\x30\x25\x20\x28\x34\x2f"
        (* bjects:  80% (4/ *);
        "\x35\x29\x0d\x43\x6f\x75\x6e\x74\x69\x6e\x67\x20\x6f\x62\x6a\x65"
        (* 5).Counting obje *);
        "\x63\x74\x73\x3a\x20\x31\x30\x30\x25\x20\x28\x35\x2f\x35\x29\x0d"
        (* cts: 100% (5/5). *);
        "\x30\x30\x32\x39\x02\x43\x6f\x75\x6e\x74\x69\x6e\x67\x20\x6f\x62"
        (* 0029.Counting ob *);
        "\x6a\x65\x63\x74\x73\x3a\x20\x31\x30\x30\x25\x20\x28\x35\x2f\x35"
        (* jects: 100% (5/5 *);
        "\x29\x2c\x20\x64\x6f\x6e\x65\x2e\x0a\x30\x30\x32\x35\x02\x43\x6f"
        (* ), done..0025.Co *);
        "\x6d\x70\x72\x65\x73\x73\x69\x6e\x67\x20\x6f\x62\x6a\x65\x63\x74"
        (* mpressing object *);
        "\x73\x3a\x20\x20\x35\x30\x25\x20\x28\x31\x2f\x32\x29\x0d\x30\x30"
        (* s:  50% (1/2).00 *);
        "\x32\x35\x02\x43\x6f\x6d\x70\x72\x65\x73\x73\x69\x6e\x67\x20\x6f"
        (* 25.Compressing o *);
        "\x62\x6a\x65\x63\x74\x73\x3a\x20\x31\x30\x30\x25\x20\x28\x32\x2f"
        (* bjects: 100% (2/ *);
        "\x32\x29\x0d\x30\x30\x32\x63\x02\x43\x6f\x6d\x70\x72\x65\x73\x73"
        (* 2).002c.Compress *);
        "\x69\x6e\x67\x20\x6f\x62\x6a\x65\x63\x74\x73\x3a\x20\x31\x30\x30"
        (* ing objects: 100 *);
        "\x25\x20\x28\x32\x2f\x32\x29\x2c\x20\x64\x6f\x6e\x65\x2e\x0a\x30"
        (* % (2/2), done..0 *);
        "\x31\x32\x35\x01\x50\x41\x43\x4b\x00\x00\x00\x02\x00\x00\x00\x03"
        (* 125.PACK........ *);
        "\x9c\x0d\x78\x9c\x9d\xcb\x31\x0e\xc2\x30\x0c\x00\xc0\x3d\xaf\xf0"
        (* ..x...1..0...=.. *);
        "\x8e\x54\x39\x6e\x6c\x12\x09\x21\x46\x16\x1e\x91\xc6\x2e\x65\x68"
        (* .T9nl..!F.....eh *);
        "\x8a\x42\xf8\x3f\x15\xfc\x80\xf1\x86\xeb\xcd\x0c\x8a\x25\x0e\x4c"
        (* .B.?.........%.L *);
        "\x47\x0a\x84\xec\x95\xe7\xc9\x4c\x12\xe5\x39\xab\x09\x62\xe0\x22"
        (* G......L..9..b.. *);
        "\xcc\x22\xea\x9e\xb9\x59\xed\x30\x63\x54\xd9\xc3\x48\x7c\x64\x8a"
        (* .....Y.0cT..H|d. *);
        "\x29\xa2\xf7\x9c\x02\x85\xb8\x67\x55\xf5\xa3\x52\xf1\x53\x76\xf9"
        (* )......gU..R.Sv. *);
        "\xdd\x97\xad\xc1\x35\xd7\x6a\x2f\xb8\xd9\x52\xad\x75\x38\x2d\x5f"
        (* ....5.j/..R.u8-_ *);
        "\x5f\xd6\x9f\x87\xad\xdd\xcf\xe0\x59\x08\x53\x18\x63\x80\x03\x12"
        (* _.......Y.S.c... *);
        "\xa2\x2b\xdb\xba\x3e\x7a\xb7\x3f\xbb\x1b\xdc\x07\xdf\x27\x40\xdc"
        (* .+..>z.?.....'@. *);
        "\xaa\x04\x78\x9c\x33\x34\x30\x30\x33\x31\x51\xc8\xcd\x2c\x4a\x4c"
        (* ..x.340031Q..,JL *);
        "\x4f\x65\x38\x5c\x3c\x23\xff\xd1\x8d\xb2\xbf\xba\x0b\x0a\x6d\xaa"
        (* Oe8\<#........m. *);
        "\xf3\xd3\xea\xae\x55\xec\xbc\x65\x88\xac\x42\x2f\x3e\x3b\xb5\xb2"
        (* ....U..e..B/>;.. *);
        "\x98\x61\x1e\x5b\x4d\xfc\x44\xde\x7e\xa9\x0b\x9b\x32\x05\x16\xa4"
        (* .a.[M.D.~...2... *);
        "\x1e\xe5\x5b\xfd\x67\x7d\x3d\x00\x51\x24\x1e\xd3\x78\xb4\xee\xfb"
        (* ..[.g}=.Q$..x... *);
        "\x37\xe3\x84\xe5\x81\xa1\xc1\x11\x58\xa5\xe6\x97\x3c\x61\x2d\xb0"
        (* 7.......X...<a-. *);
        "\x8c\x78\x9c\x5b\xc2\x74\x92\x71\xc2\x09\x46\x2e\x00\x0e\x95\x02"
        (* .x.[.t.q..F..... *);
        "\xd4\x2a\x24\x0d\xf3\x07\x00\x87\xe7\x90\x77\xdd\x6d\x50\x2a\x6a"
        (* ..$.......w.mP.j *);
        "\x1e\x23\x9a\xeb\x30\x30\x33\x61\x02\x54\x6f\x74\x61\x6c\x20\x33"
        (* .#..003a.Total 3 *);
        "\x20\x28\x64\x65\x6c\x74\x61\x20\x31\x29\x2c\x20\x72\x65\x75\x73"
        (*  (delta 1), reus *);
        "\x65\x64\x20\x33\x20\x28\x64\x65\x6c\x74\x61\x20\x31\x29\x2c\x20"
        (* ed 3 (delta 1),  *);
        "\x70\x61\x63\x6b\x2d\x72\x65\x75\x73\x65\x64\x20\x30\x0a\x30\x30"
        (* pack-reused 0.00 *);
        "\x30\x36\x01\x66\x30\x30\x30\x30" (* 06.f0000 *);
      ]
    in
    create_new_git_store sw >>= fun (access, store) ->
    let { path; _ } = store_prj store in
    let pack, index =
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.pack")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.pack")
    |> Lwt.return
    >>? fun () ->
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.idx")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.idx")
    |> Lwt.return
    >>? fun () ->
    update_testzone_0 store >>? fun () ->
    let ctx = ctx_with_payloads (payloads, ignore) in
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    Smart_git.Endpoint.of_string "git://localhost/not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Git_sync.fetch ~ctx ~capabilities access store endpoint `All pack index
      ~src:tmp0 ~dst:tmp1 ~idx:tmp2
  in
  run () >>= function
  | Ok (`Pack _) -> Lwt.return_unit
  | Ok `Empty -> Alcotest.failf "Unexpected empty fetch"
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let ( <.> ) f g x = f (g x)

let run_git_upload_pack ?(tmps_exit = true) store ic oc =
  let { path; _ } = store_prj store in
  let process =
    OS.Dir.with_current path @@ fun () ->
    let tee = Cmd.(v "tee" % Fpath.to_string ic) in
    let cat = Cmd.(v "cat" % Fpath.to_string oc) in
    let git_upload_pack = Cmd.(v "git-upload-pack" % Fpath.to_string path) in
    let pipe () =
      OS.Cmd.run_out cat |> OS.Cmd.out_run_in >>= fun cat ->
      OS.Cmd.run_io git_upload_pack cat |> OS.Cmd.out_run_in >>= fun git ->
      OS.Cmd.run_in tee git
    in
    match Unix.fork () with
    | 0 -> (
        match pipe () with
        | Ok () ->
            Tmp_dirs.are_valid := tmps_exit;
            Logs.debug (fun m -> m "git-upload-pack terminated properly.");
            exit 1
        | Error (`Msg err) -> Alcotest.failf "git-upload-pack: %s" err)
    | _ ->
        Logs.app (fun m -> m "git-upload-pack launched!");
        Lwt.return_unit
  in
  R.failwith_error_msg <.> process

let always v _ = v

let test_ssh () =
  Alcotest_lwt.test_case "clone over ssh" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  (* XXX(dinosaure): This test does not require SSH but the underlying process
     is a call to [git-upload-pack]. We do the same without encryption. *)
  let run () =
    create_new_git_store sw >>= fun (_access, store0) ->
    let { path; _ } = store_prj store0 in
    let pack = Fpath.(path / ".git" / "objects" / "pack") in
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.pack")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.pack")
    |> Lwt.return
    >>? fun () ->
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.idx")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.idx")
    |> Lwt.return
    >>? fun () ->
    update_testzone_0 store0 >>? fun () ->
    with_fifo "git-upload-pack-ic-%s" |> Lwt.return >>? fun ic_fifo ->
    with_fifo "git-upload-pack-oc-%s" |> Lwt.return >>? fun oc_fifo ->
    let process = run_git_upload_pack store0 ic_fifo oc_fifo in
    process () >>= fun () ->
    (* XXX(dinosaure): order of temporary files is important where [process] do
       a [fork]. [Bos] keeps a global set of all temporary files and it removes
       them at the [exit] call.

       TODO(dinosaure): move the initialisation of [store0] into the child
       process. *)
    create_new_git_store sw >>= fun (access, store1) ->
    let { path; _ } = store_prj store0 in
    let pack, index =
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    let capabilities = [] in
    let ctx = ctx_with_fifo ic_fifo oc_fifo in
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    Smart_git.Endpoint.of_string "git@localhost:not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Logs.app (fun m -> m "Waiting git-upload-pack.");
    Logs.app (fun m -> m "Start to fetch repository with SSH.");
    Git_sync.fetch ~ctx ~capabilities access store1 endpoint
      (`Some [ Ref.v "HEAD" ])
      pack index ~src:tmp0 ~dst:tmp1 ~idx:tmp2
  in
  run () >>= function
  | Ok `Empty -> Alcotest.failf "Unexpected empty fetch"
  | Ok (`Pack _) -> Lwt.return_unit
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let update_testzone_1 store =
  let { path; _ } = store_prj store in
  let update =
    OS.Dir.with_current path @@ fun () ->
    OS.Cmd.run
      Cmd.(
        v "git"
        % "update-ref"
        % "refs/heads/master"
        % "b88599cb4217c175110f6e2a810079d954524814")
  in
  match R.join (update ()) with
  | Ok () -> Lwt.return_ok ()
  | Error err -> Lwt.return_error err

let test_negotiation_ssh () =
  Alcotest_lwt.test_case "fetch over ssh" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  (* XXX(dinosaure): This test does not require SSH but the underlying process
     is a call to [git-upload-pack]. We do the same without encryption. *)
  let run () =
    create_new_git_store sw >>= fun (_access, store0) ->
    let { path; _ } = store_prj store0 in
    let pack = Fpath.(path / ".git" / "objects" / "pack") in
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-1.pack")
      Fpath.(pack / "pack-02e2924e51b624461d8ee6706a455c5ce1a6ad80.pack")
    |> Lwt.return
    >>? fun () ->
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-1.idx")
      Fpath.(pack / "pack-02e2924e51b624461d8ee6706a455c5ce1a6ad80.idx")
    |> Lwt.return
    >>? fun () ->
    update_testzone_1 store0 >>? fun () ->
    with_fifo "git-upload-pack-ic-%s" |> Lwt.return >>? fun ic_fifo ->
    with_fifo "git-upload-pack-oc-%s" |> Lwt.return >>? fun oc_fifo ->
    let process = run_git_upload_pack store0 ic_fifo oc_fifo in
    process () >>= fun () ->
    (* XXX(dinosaure): order of temporary files is important where [process] do
       a [fork]. [Bos] keeps a global set of all temporary files and it removes
       them at the [exit] call.

       TODO(dinosaure): move the initialisation of [store0] into the child
       process. *)
    create_new_git_store sw >>= fun (access, store1) ->
    let { path; _ } = store_prj store1 in
    let pack, index =
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.pack")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.pack")
    |> Lwt.return
    >>? fun () ->
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.idx")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.idx")
    |> Lwt.return
    >>? fun () ->
    update_testzone_0 store1 >>? fun () ->
    let capabilities =
      [ `Side_band_64k; `Multi_ack_detailed; `Thin_pack; `Ofs_delta ]
    in
    let ctx = ctx_with_fifo ic_fifo oc_fifo in
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    Smart_git.Endpoint.of_string "git@localhost:not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Logs.app (fun m -> m "Waiting git-upload-pack.");
    Logs.app (fun m -> m "Start to fetch repository with SSH.");
    Git_sync.fetch ~ctx ~capabilities access store1 endpoint
      (`Some [ Ref.v "HEAD" ])
      pack index ~src:tmp0 ~dst:tmp1 ~idx:tmp2
  in
  run () >>= function
  | Ok `Empty -> Alcotest.failf "Unexpected empty fetch"
  | Ok (`Pack _) -> Lwt.return_unit
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let run_git_receive_pack store ic oc =
  let { path; _ } = store_prj store in
  let process =
    OS.Dir.with_current path @@ fun () ->
    let tee = Cmd.(v "tee" % Fpath.to_string ic) in
    let cat = Cmd.(v "cat" % Fpath.to_string oc) in
    let git_receive_pack = Cmd.(v "git-receive-pack" % ".") in
    let pipe () =
      OS.Cmd.run
        Cmd.(
          v "git" % "config" % "--add" % "receive.denyCurrentBranch" % "ignore")
      >>= fun () ->
      OS.Cmd.run_out cat |> OS.Cmd.out_run_in >>= fun cat ->
      OS.Cmd.run_io git_receive_pack cat |> OS.Cmd.out_run_in >>= fun git ->
      OS.Cmd.run_in tee git
    in
    match Unix.fork () with
    | 0 -> (
        match pipe () with
        | Ok () ->
            Tmp_dirs.are_valid := false;
            Logs.debug (fun m -> m "git-receive-pack terminated properly.");
            exit 1
        | Error (`Msg err) -> Alcotest.failf "git-upload-pack: %s" err)
    | _ ->
        Logs.app (fun m -> m "git-receive-pack launched!");
        Lwt.return_unit
  in
  R.failwith_error_msg <.> process

let test_push_ssh () =
  Alcotest_lwt.test_case "push over ssh" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  (* XXX(dinosaure): This test does not require SSH but the underlying process
     is a call to [git-upload-pack]. We do the same without encryption. *)
  let run () =
    create_new_git_store sw >>= fun (_access, store0) ->
    let { path; _ } = store_prj store0 in
    let pack = Fpath.(path / ".git" / "objects" / "pack") in
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.pack")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.pack")
    |> Lwt.return
    >>? fun () ->
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.idx")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.idx")
    |> Lwt.return
    >>? fun () ->
    update_testzone_0 store0 >>? fun () ->
    with_fifo "git-receive-pack-ic-%s" |> Lwt.return >>? fun ic_fifo ->
    with_fifo "git-receive-pack-oc-%s" |> Lwt.return >>? fun oc_fifo ->
    let process = run_git_receive_pack store0 ic_fifo oc_fifo in
    process () >>= fun () ->
    create_new_git_push_store sw >>= fun (access, store1) ->
    let { path; _ } = store_prj store1 in
    let pack = Fpath.(path / ".git" / "objects" / "pack") in
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-1.pack")
      Fpath.(pack / "pack-02e2924e51b624461d8ee6706a455c5ce1a6ad80.pack")
    |> Lwt.return
    >>? fun () ->
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-1.idx")
      Fpath.(pack / "pack-02e2924e51b624461d8ee6706a455c5ce1a6ad80.idx")
    |> Lwt.return
    >>? fun () ->
    update_testzone_1 store1 >>? fun () ->
    let capabilities = [ `Report_status; `Side_band_64k ] in
    let ctx = ctx_with_fifo ic_fifo oc_fifo in
    Smart_git.Endpoint.of_string "git@localhost:not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Git_sync.push ~ctx ~capabilities access store1 endpoint
      [ `Update (Ref.v "refs/heads/master", Ref.v "refs/heads/master") ]
    >>? fun () ->
    let { path; _ } = store_prj store0 in
    Lwt.return_ok path
  in
  run () >>= function
  | Ok path -> (
      let run =
        OS.Dir.with_current path @@ fun () ->
        let cmd = Cmd.(v "git" % "show-ref" % "--heads" % "master" % "-s") in
        let run = OS.Cmd.run_out cmd in
        OS.Cmd.out_lines ~trim:true run
      in
      match R.join (run ()) with
      | Ok ([ hash ], _) ->
          Alcotest.(check string)
            "push" hash "b88599cb4217c175110f6e2a810079d954524814";
          Lwt.return_unit
      | Ok (hashes, _) ->
          Alcotest.failf "refs/heads/master has multiple hashes: %a"
            Fmt.(Dump.list string)
            hashes
      | Error (`Msg err) -> Alcotest.failf "git-show-ref: %s" err)
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let load_file filename =
  let ic = open_in_bin filename in
  let ln = in_channel_length ic in
  let rs = Bytes.create ln in
  really_input ic rs 0 ln;
  close_in ic;
  Bytes.unsafe_to_string rs

let test_negotiation_http () =
  Alcotest_lwt.test_case "fetch over http" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    let capabilities =
      [ `Side_band_64k; `Multi_ack_detailed; `Thin_pack; `Ofs_delta; `No_done ]
    in
    create_new_git_store sw >>= fun (access, store) ->
    let { path; _ } = store_prj store in
    let pack, index =
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.pack")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.pack")
    |> Lwt.return
    >>? fun () ->
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.idx")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.idx")
    |> Lwt.return
    >>? fun () ->
    update_testzone_0 store >>? fun () ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    Smart_git.Endpoint.of_string "http://localhost/not-found.git" |> Lwt.return
    >>? fun endpoint ->
    let advertised_refs = load_file "GET" in
    let clone = load_file "POST" in
    let fake_http_edn, fake_http =
      Mimic.register ~name:"fake-http" (module HTTP)
    in
    let handshake ~uri0:_ ~uri1:_ flow =
      let module M = (val Mimic.repr fake_http) in
      match flow with
      | M.T flow ->
          flow.state <- Get;
          Lwt.return_unit
      | _ -> Lwt.return_unit
    in
    let k0 git_scheme git_transmission =
      match git_scheme, git_transmission with
      | (`HTTP | `HTTPS), `HTTP _ -> Lwt.return_some (advertised_refs, clone)
      | _ -> Lwt.return_none
    in
    let k1 git_scheme =
      match git_scheme with
      | `HTTP | `HTTPS ->
          Lwt.return_some
            (`HTTP (Uri.of_string "http://localhost/not-found.git", handshake))
      | _ -> Lwt.return_none
    in
    let ctx =
      Mimic.fold fake_http_edn
        Mimic.Fun.[ req Smart_git.git_scheme; req Smart_git.git_transmission ]
        ~k:k0 Mimic.empty
    in
    let ctx =
      Mimic.fold Smart_git.git_transmission
        Mimic.Fun.[ req Smart_git.git_scheme ]
        ~k:k1 ctx
    in
    Git_sync.fetch ~ctx ~capabilities access store endpoint `All pack index
      ~src:tmp0 ~dst:tmp1 ~idx:tmp2
  in
  run () >>= function
  | Ok (`Pack _) -> Lwt.return_unit
  | Ok `Empty -> Alcotest.failf "Unexpected empty fetch"
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let test_partial_clone_ssh () =
  Alcotest_lwt.test_case "partial clone over ssh" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    create_new_git_store sw >>= fun (_access, store0) ->
    let { path; _ } = store_prj store0 in
    let pack = Fpath.(path / ".git" / "objects" / "pack") in
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.pack")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.pack")
    |> Lwt.return
    >>? fun () ->
    OS.Path.link
      ~target:(Fpath.v "pack-testzone-0.idx")
      Fpath.(pack / "pack-4aae6e55c118eb1ab3d1e2cd5a7e4857faa23d4e.idx")
    |> Lwt.return
    >>? fun () ->
    update_testzone_0 store0 >>? fun () ->
    with_fifo "git-upload-pack-ic-%s" |> Lwt.return >>? fun ic_fifo ->
    with_fifo "git-upload-pack-oc-%s" |> Lwt.return >>? fun oc_fifo ->
    let process = run_git_upload_pack store0 ic_fifo oc_fifo in
    process () >>= fun () ->
    (* XXX(dinosaure): order of temporary files is important where [process] do
       a [fork]. [Bos] keeps a global set of all temporary files and it removes
       them at the [exit] call.

       TODO(dinosaure): move the initialisation of [store0] into the child
       process. *)
    create_new_git_store sw >>= fun (access, store1) ->
    let { path; _ } = store_prj store0 in
    let pack, index =
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    let capabilities = [] in
    let ctx = ctx_with_fifo ic_fifo oc_fifo in
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    Smart_git.Endpoint.of_string "git@localhost:not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Logs.app (fun m -> m "Waiting git-upload-pack.");
    Logs.app (fun m -> m "Start to fetch repository with SSH.");
    Git_sync.fetch ~ctx ~capabilities access store1 endpoint ~deepen:(`Depth 1)
      (`Some [ Ref.v "HEAD" ])
      pack index ~src:tmp0 ~dst:tmp1 ~idx:tmp2
    >>? function
    | `Empty -> Alcotest.failf "Unexpected empty fetch"
    | `Pack _ ->
        Store_backend.shallowed lwt store1 |> Scheduler.prj >>= fun shallowed ->
        Alcotest.(check (list uid))
          "shallowed" shallowed
          [ Uid.of_hex "f08d64523257528980115942481d5ddd13d2c1ba0000" ];
        Lwt.return_ok ()
  in
  run () >>= function
  | Ok () -> Lwt.return_unit
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let test_partial_fetch_ssh () =
  Alcotest_lwt.test_case "partial fetch" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let fill0 () =
    create_new_git_store sw >>= fun (access, store) ->
    let { path; _ } = store_prj store in
    let fiber =
      let open Rresult in
      OS.Dir.with_current path @@ fun () ->
      OS.Cmd.run Cmd.(v "git" % "config" % "user.name" % "test") >>= fun () ->
      OS.Cmd.run
        Cmd.(v "git" % "config" % "user.email" % "pseudo@pseudo.invalid")
      >>= fun () ->
      OS.Cmd.run Cmd.(v "touch" % "foo") >>= fun () ->
      OS.Cmd.run Cmd.(v "git" % "add" % "foo") >>= fun () ->
      OS.Cmd.run Cmd.(v "git" % "commit" % "-m" % ".")
    in
    (Lwt.return <.> R.join) (fiber ()) >>? fun () ->
    Lwt.return_ok (access, store)
  in
  let fill1 store =
    let { path; _ } = store_prj store in
    let fiber =
      let open Rresult in
      OS.Dir.with_current path @@ fun () ->
      OS.Cmd.run Cmd.(v "git" % "config" % "user.name" % "test") >>= fun () ->
      OS.Cmd.run
        Cmd.(v "git" % "config" % "user.email" % "pseudo@pseudo.invalid")
      >>= fun () ->
      OS.Cmd.run Cmd.(v "touch" % "bar") >>= fun () ->
      OS.Cmd.run Cmd.(v "git" % "add" % "bar") >>= fun () ->
      OS.Cmd.run Cmd.(v "git" % "commit" % "-m" % ".") >>= fun () ->
      OS.Cmd.run Cmd.(v "git" % "rm" % "foo") >>= fun () ->
      OS.Cmd.run Cmd.(v "git" % "commit" % "-m" % ".")
    in
    (Lwt.return <.> R.join) (fiber ())
  in
  let capabilities =
    [ `Side_band_64k; `Multi_ack_detailed; `Thin_pack; `Ofs_delta ]
  in
  let endpoint =
    R.get_ok (Smart_git.Endpoint.of_string "git@localhost:not-found.git")
  in
  let run () =
    fill0 () >>? fun (_access, store0) ->
    with_fifo "git-upload-pack-ic-%s" |> Lwt.return >>? fun ic_fifo ->
    with_fifo "git-upload-pack-oc-%s" |> Lwt.return >>? fun oc_fifo ->
    let process = run_git_upload_pack ~tmps_exit:false store0 ic_fifo oc_fifo in
    process () >>= fun () ->
    create_new_git_store sw >>= fun (access, store1) ->
    let ctx = ctx_with_fifo ic_fifo oc_fifo in
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
    OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
    OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
    let pack, index =
      let { path; _ } = store_prj store1 in
      ( Fpath.(path / ".git" / "objects" / "pack"),
        Fpath.(path / ".git" / "objects" / "pack") )
    in
    Logs.app (fun m -> m "Waiting git-upload-pack.");
    Logs.app (fun m -> m "Start to fetch repository with SSH.");
    Git_sync.fetch ~ctx ~capabilities access store1 endpoint ~deepen:(`Depth 1)
      (`Some [ Ref.v "HEAD" ])
      pack index ~src:tmp0 ~dst:tmp1 ~idx:tmp2
    >>? function
    | `Empty -> Alcotest.failf "Unexpected empty fetch"
    | `Pack (uid, refs) -> (
        let { path; _ } = store_prj store1 in
        let dst =
          Fpath.(
            path
            / ".git"
            / "objects"
            / "pack"
            / Fmt.str "pack-%a.pack" Uid.pp uid)
        in
        OS.Path.move tmp1 dst |> Lwt.return >>? fun () ->
        let dst =
          Fpath.(
            path
            / ".git"
            / "objects"
            / "pack"
            / Fmt.str "pack-%a.idx" Uid.pp uid)
        in
        OS.Path.move tmp2 dst |> Lwt.return >>? fun () ->
        let update (refname, uid) =
          OS.Dir.with_current path @@ fun () ->
          OS.Cmd.run
            Cmd.(
              v "git" % "update-ref" % Ref.to_string refname % Uid.to_hex uid)
        in
        List.fold_right
          (fun v -> function Ok a -> R.join (update v a) | err -> err)
          refs (Ok ())
        |> Lwt.return
        >>? fun () ->
        fill1 store0 >>? fun () ->
        with_fifo "git-upload-pack-ic-%s" |> Lwt.return >>? fun ic_fifo ->
        with_fifo "git-upload-pack-oc-%s" |> Lwt.return >>? fun oc_fifo ->
        let process =
          run_git_upload_pack ~tmps_exit:false store0 ic_fifo oc_fifo
        in
        process () >>= fun () ->
        let ctx = ctx_with_fifo ic_fifo oc_fifo in
        OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp0 ->
        OS.File.tmp "pack-%s.pack" |> Lwt.return >>? fun tmp1 ->
        OS.File.tmp "pack-%s.idx" |> Lwt.return >>? fun tmp2 ->
        Logs.app (fun m -> m "Waiting git-upload-pack.");
        Logs.app (fun m -> m "Start to fetch repository with SSH.");
        Git_sync.fetch ~ctx ~capabilities access store1 endpoint
          ~deepen:(`Depth 1)
          (`Some [ Ref.v "HEAD" ])
          pack index ~src:tmp0 ~dst:tmp1 ~idx:tmp2
        >>? function
        | `Empty -> Alcotest.failf "Unexpected empty fetch"
        | `Pack _ ->
            Store_backend.shallowed lwt store1 |> Scheduler.prj
            >>= fun shallowed ->
            Alcotest.(check int) "2 shallowed commits" (List.length shallowed) 2;
            Lwt.return_ok ())
  in
  run () >>= function
  | Ok v -> Lwt.return v
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let make_one_commit path =
  OS.Dir.with_current path @@ fun () ->
  OS.Cmd.run Cmd.(v "git" % "config" % "user.name" % "test") >>= fun () ->
  OS.Cmd.run Cmd.(v "git" % "config" % "user.email" % "pseudo@peudo.invalid")
  >>= fun () ->
  OS.File.write Fpath.(v "foo") "" >>= fun () ->
  OS.Cmd.run Cmd.(v "git" % "add" % "foo") >>= fun () ->
  OS.Cmd.run Cmd.(v "git" % "commit" % "-m" % ".") >>= fun () -> R.ok ()

let test_push_empty () =
  Alcotest_lwt.test_case "push to empty over ssh" `Quick @@ fun sw () ->
  let open Lwt.Infix in
  let run () =
    create_new_git_store sw >>= fun (_access, store0) ->
    with_fifo "git-receive-pack-ic-%s" |> Lwt.return >>? fun ic_fifo ->
    with_fifo "git-receive-pack-oc-%s" |> Lwt.return >>? fun oc_fifo ->
    let process = run_git_receive_pack store0 ic_fifo oc_fifo in
    process () >>= fun () ->
    create_new_git_push_store sw >>= fun (access, store1) ->
    let { path; _ } = store_prj store1 in
    make_one_commit path () |> R.join |> Lwt.return >>? fun () ->
    let capabilities = [ `Report_status; `Side_band_64k ] in
    let ctx = ctx_with_fifo ic_fifo oc_fifo in
    Smart_git.Endpoint.of_string "git@localhost:not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Git_sync.push ~ctx ~capabilities access store1 endpoint
      [ `Update (Ref.v "refs/heads/master", Ref.v "refs/heads/master") ]
  in
  run () >>= function
  | Ok () -> Lwt.return_unit
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error `Invalid_flow -> Alcotest.fail "Invalid flow"

let simple_push =
  [
    "008e0000000000000000000000000000000000000000 \
     refs/heads/master\000report-status delete-refs side-band-64k quiet atomic \
     ofs-delta agent=git/2.27.0";
    "0000";
  ]

let capability = Alcotest.testable Smart.Capability.pp Smart.Capability.equal

let test_push_capabilities () =
  Alcotest_lwt.test_case "push capabilities" `Quick @@ fun _sw () ->
  let open Lwt.Infix in
  let module Sync = Git.Mem.Sync (Git.Mem.Store) in
  let output = ref None in
  let ctx =
    ctx_with_payloads ~transmission:`Exec
      (simple_push, fun v -> output := Some v)
  in
  let capabilities =
    [
      `Side_band_64k;
      `Multi_ack_detailed;
      `Thin_pack;
      `Ofs_delta;
      `Agent "ocaml-git";
    ]
  in
  let author =
    {
      Git.User.name = "Romain Calascibetta";
      email = "romain.calascibetta@gmail.com";
      date =
        (let ptime = Ptime.unsafe_of_d_ps (Ptime_clock.now_d_ps ()) in
         let tz =
           match Ptime_clock.current_tz_offset_s () with
           | Some s ->
               let sign = if s < 0 then `Minus else `Plus in
               let hours = s / 3600 in
               let minutes = s mod 3600 / 60 in
               Some { Git.User.sign; hours; minutes }
           | None -> None
         in
         Int64.of_float (Ptime.to_float_s ptime), tz);
    }
  in
  let tree0 = Git.Mem.Store.Value.(tree (Tree.v [])) in
  let commit0 root =
    Git.Mem.Store.Value.(
      commit
        (Commit.make ~parents:[] ~tree:root ~author ~committer:author (Some ".")))
  in
  let fiber =
    Git.Mem.Store.v (Fpath.v "/") >|= store_err >>? fun store ->
    Git.Mem.Store.write store tree0 >|= store_err >>? fun (root, _) ->
    Git.Mem.Store.write store (commit0 root) >|= store_err
    >>? fun (commit, _) ->
    Git.Mem.Store.Ref.write store Git.Reference.master
      (Git.Reference.uid commit)
    >|= store_err
    >>? fun _ ->
    Smart_git.Endpoint.of_string "git@localhost:not-found.git" |> Lwt.return
    >>? fun endpoint ->
    Sync.push ~ctx ~capabilities endpoint store
      [ `Update (Git.Reference.master, Git.Reference.master) ]
    >|= sync_err
  in
  fiber >>= function
  | Ok () -> (
      let[@warning "-8"] (Some v) = !output in
      let decoder = Pkt_line.Decoder.of_string (Cstruct.to_string v) in
      let ctx = Smart.Context.with_decoder ~my_caps:capabilities decoder in
      match Smart.recv ctx Smart.recv_commands with
      | Smart.Return (Some v) ->
          let c = Smart.Commands.capabilities v in
          Alcotest.(check (list capability)) "capabilities" c capabilities;
          Lwt.return_unit
      | _ -> Alcotest.failf "Cannot parse: %S" (Cstruct.to_string v))
  | Error (`Exn exn) -> Alcotest.failf "%s" (Printexc.to_string exn)
  | Error (#Mimic.error as err) -> Alcotest.failf "%a" Mimic.pp_error err
  | Error (`Sync err) -> Alcotest.failf "%a" Sync.pp_error err
  | Error (`Store err) -> Alcotest.failf "%a" Git.Mem.Store.pp_error err

let update_testzone_1 store =
  let { path; _ } = store_prj store in
  let update =
    OS.Dir.with_current path @@ fun () ->
    OS.Cmd.run
      Cmd.(
        v "git"
        % "update-ref"
        % "refs/heads/master"
        % "b88599cb4217c175110f6e2a810079d954524814")
  in
  match R.join (update ()) with
  | Ok () -> Lwt.return_ok ()
  | Error err -> Lwt.return_error err

let test =
  Alcotest_lwt.run "smart"
    [
      ( "regression",
        [
          test_empty_clone ();
          test_simple_clone ();
          test_simple_push ();
          test_push_error ();
          test_fetch_empty ();
          test_negotiation ();
          test_ssh ();
          test_negotiation_ssh ();
          test_push_ssh ();
          test_negotiation_http ();
          test_partial_clone_ssh ();
          test_partial_fetch_ssh ();
          test_sync_fetch ();
          test_push_empty ();
          test_push_capabilities ();
        ] );
    ]

let tmp = "tmp"

let () =
  let fiber =
    OS.Dir.current () >>= fun current ->
    OS.Dir.create Fpath.(current / tmp) >>= fun _ -> R.ok Fpath.(current / tmp)
  in
  let tmp = R.failwith_error_msg fiber in
  OS.Dir.set_default_tmp tmp;
  Lwt_main.run test
