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

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let () =
  let random_seed = seed in
  Fmt.pr "Random: %a.\n%!" Fmt.(Dump.array int) random_seed;
  Random.full_init random_seed

module Store = Git_unix.Make (Digestif.SHA1)
module Test = Test_store.Make (Digestif.SHA1) (Store)

let ( <.> ) f g x = f (g x)

let git_object_exist path hash =
  let open Rresult in
  R.join
  <.> Bos.OS.Dir.with_current path @@ fun () ->
      Bos.OS.Cmd.run
        Bos.Cmd.(v "git" % "cat-file" % "-e" % Digestif.SHA1.to_hex hash)
      >>= fun () ->
      Alcotest.(check pass) (Fmt.str "%a" Digestif.SHA1.pp hash) () ();
      R.ok ()

let check_blobs_with_git =
  Alcotest.test_case "blobs" `Quick @@ fun store ->
  let root = Store.root store in
  let open Rresult in
  let run () =
    git_object_exist root (Store.Value.Blob.digest Test.v0) () >>= fun () ->
    git_object_exist root (Store.Value.Blob.digest Test.v1) () >>= fun () ->
    git_object_exist root (Store.Value.Blob.digest Test.v2) () >>= fun () ->
    R.ok ()
  in
  match run () with
  | Ok () -> Alcotest.(check pass) "git" () ()
  | Error err -> Alcotest.failf "%a" Rresult.R.pp_msg err

let check_trees_with_git =
  Alcotest.test_case "trees" `Quick @@ fun store ->
  let root = Store.root store in
  let open Rresult in
  let run () =
    git_object_exist root (Store.Value.Tree.digest Test.t0) () >>= fun () ->
    git_object_exist root (Store.Value.Tree.digest Test.t1) () >>= fun () ->
    git_object_exist root (Store.Value.Tree.digest Test.t2) () >>= fun () ->
    git_object_exist root (Store.Value.Tree.digest Test.t3) () >>= fun () ->
    git_object_exist root (Store.Value.Tree.digest Test.t4) () >>= fun () ->
    git_object_exist root (Store.Value.Tree.digest Test.t5) () >>= fun () ->
    R.ok ()
  in
  match run () with
  | Ok () -> Alcotest.(check pass) "git" () ()
  | Error err -> Alcotest.failf "%a" Rresult.R.pp_msg err

let check_commits_with_git =
  Alcotest.test_case "commits" `Quick @@ fun store ->
  let root = Store.root store in
  let open Rresult in
  let run () =
    git_object_exist root (Store.Value.Commit.digest Test.c1) () >>= fun () ->
    git_object_exist root (Store.Value.Commit.digest Test.c2) () >>= fun () ->
    git_object_exist root (Store.Value.Commit.digest Test.c3) () >>= fun () ->
    git_object_exist root (Store.Value.Commit.digest Test.c4) () >>= fun () ->
    git_object_exist root (Store.Value.Commit.digest Test.c5) () >>= fun () ->
    R.ok ()
  in
  match run () with
  | Ok () -> Alcotest.(check pass) "git" () ()
  | Error err -> Alcotest.failf "%a" Rresult.R.pp_msg err

let check_tags_with_git =
  Alcotest.test_case "tags" `Quick @@ fun store ->
  let root = Store.root store in
  let open Rresult in
  let run () =
    git_object_exist root (Store.Value.Tag.digest Test.tt1) () >>= fun () ->
    git_object_exist root (Store.Value.Tag.digest Test.tt2) () >>= fun () ->
    R.ok ()
  in
  match run () with
  | Ok () -> Alcotest.(check pass) "git" () ()
  | Error err -> Alcotest.failf "%a" Rresult.R.pp_msg err

let git_get_reference path refname =
  let open Rresult in
  R.join
  <.> Bos.OS.Dir.with_current path @@ fun () ->
      let out =
        Bos.OS.Cmd.run_out
          Bos.Cmd.(
            v "git" % "show-ref" % "--hash" % Git.Reference.to_string refname)
      in
      Bos.OS.Cmd.out_string out >>| fun (hash, _) -> Digestif.SHA1.of_hex hash

let check_references_with_git =
  Alcotest.test_case "references" `Quick @@ fun store ->
  let root = Store.root store in
  let open Rresult in
  let run () =
    git_get_reference root Test.r1 () >>= fun k ->
    Alcotest.(check Test.hash) "r1" k (Store.Value.Commit.digest Test.c1);
    git_get_reference root Test.r2 () >>= fun k ->
    Alcotest.(check Test.hash) "r2" k (Store.Value.Commit.digest Test.c2);
    R.ok ()
  in
  match run () with
  | Ok () -> Alcotest.(check pass) "git" () ()
  | Error err -> Alcotest.failf "%a" Rresult.R.pp_msg err

let ( >>? ) x f =
  let open Lwt.Infix in
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

let packed_refs =
  Alcotest.test_case "packed-refs" `Quick @@ fun store ->
  let open Rresult in
  let path = Store.root store in
  let fiber =
    let open Rresult in
    R.join
    <.> Bos.OS.Dir.with_current path @@ fun () ->
        Bos.OS.Cmd.run Bos.Cmd.(v "git" % "gc")
  in
  let () = Rresult.R.get_ok (fiber ()) in
  let fiber () =
    Store.v (Store.root store) >>? fun store ->
    Store.Ref.resolve store Test.r1 >>? fun v1 ->
    Store.Ref.resolve store Test.r2 >>? fun v2 -> Lwt.return_ok (v1, v2)
  in
  match Lwt_main.run (fiber ()) with
  | Ok (v1, v2) ->
      Alcotest.(check Test.hash) "r1" v1 (Store.Value.Commit.digest Test.c1);
      Alcotest.(check Test.hash) "r2" v2 (Store.Value.Commit.digest Test.c2)
  | Error err -> Alcotest.failf "%a" Store.pp_error err

let reference = Alcotest.testable Git.Reference.pp Git.Reference.equal

let git_init =
  Alcotest.test_case "init" `Quick @@ fun _store ->
  let fiber root =
    Store.v root >>? fun store ->
    Store.Ref.read store Git.Reference.head >>? function
    | Git.Reference.Ref v ->
        Alcotest.(check reference) "head" v Git.Reference.master;
        Lwt.return_ok ()
    | v ->
        Alcotest.failf "Invalid HEAD reference value: %a"
          (Git.Reference.pp_contents ~pp:Store.Hash.pp)
          v
  in
  let open Rresult in
  match
    Bos.OS.Dir.tmp "git-%s" >>= fun root ->
    Lwt_main.run (fiber root) >>= fun () ->
    ( Bos.OS.Dir.with_current root @@ fun () ->
      Bos.OS.Cmd.run_out Bos.Cmd.(v "git" % "symbolic-ref" % "HEAD")
      |> Bos.OS.Cmd.out_string ~trim:true )
      ()
    |> R.join
  with
  | Ok (v, (_, `Exited 0)) ->
      let v = Git.Reference.v v in
      Alcotest.(check reference) "head" v Git.Reference.master
  | Ok (_, _) -> Alcotest.failf "Error for 'git symbolic-ref HEAD'"
  | Error err -> Alcotest.failf "%a" Store.pp_error err

let pack_file =
  Alcotest.test_case "pack-file" `Quick @@ fun store ->
  let open Rresult in
  let path = Store.root store in
  let fiber =
    let open Rresult in
    R.join
    <.> Bos.OS.Dir.with_current path @@ fun () ->
        Bos.OS.Cmd.run Bos.Cmd.(v "git" % "repack")
    (* XXX(dinosaure): to be sure that any Git objects are packed. *)
  in
  let () = Rresult.R.get_ok (fiber ()) in
  let fiber () =
    let open Lwt.Infix in
    Store.v (Store.root store) >>? fun store ->
    Store.mem store (Store.Value.Commit.digest Test.c1) >>= fun r0 ->
    Store.mem store (Store.Value.Commit.digest Test.c2) >>= fun r1 ->
    Store.mem store (Store.Value.Commit.digest Test.c3) >>= fun r2 ->
    Store.mem store (Store.Value.Commit.digest Test.c4) >>= fun r3 ->
    Store.mem store (Store.Value.Commit.digest Test.c5) >>= fun r4 ->
    Alcotest.(check bool) "c1" r0 true;
    Alcotest.(check bool) "c2" r1 true;
    Alcotest.(check bool) "c3" r2 true;
    Alcotest.(check bool) "c4" r3 true;
    Alcotest.(check bool) "c5" r4 true;
    Lwt.return_ok ()
  in
  match Lwt_main.run (fiber ()) with
  | Ok () -> ()
  | Error err -> Alcotest.failf "%a" Store.pp_error err

let author () =
  {
    Git.User.name = "Romain Calascibetta";
    email = "romain.calascibetta@gmail.com";
    date =
      (let ptime = Ptime.unsafe_of_d_ps (Ptime_clock.now_d_ps ()) in
       let tz =
         match Ptime_clock.current_tz_offset_s () with
         | Some s ->
             let sign = if s < 0 then `Minus else `Plus in
             let s = abs s in
             let hours = s / 3600 in
             let minutes = s mod 3600 / 60 in
             Some { Git.User.sign; hours; minutes }
         | None -> None
       in
       Int64.of_float (Ptime.to_float_s ptime), tz);
  }

let empty_commit =
  Alcotest.test_case "empty commit" `Quick @@ fun store ->
  let empty_tree = Store.Value.(tree Tree.(v [])) in
  let c0 root parent =
    let author = author () in
    Store.Value.(
      commit
        (Commit.make ~tree:root ~author ~committer:author ~parents:[ parent ]
           None))
  in
  let path = Store.root store in
  let fiber0 =
    let open Rresult in
    R.join
    <.> Bos.OS.Dir.with_current path @@ fun () ->
        Bos.OS.Cmd.run Bos.Cmd.(v "git" % "config" % "user.name" % "test")
        >>= fun () ->
        Bos.OS.Cmd.run
          Bos.Cmd.(v "git" % "config" % "user.email" % "pseudo@pseudo.invalid")
        >>= fun () ->
        Bos.OS.Cmd.run
          Bos.Cmd.(v "git" % "commit" % "--allow-empty" % "-m" % ".")
  in
  let fiber1 () =
    Store.Ref.resolve store Git.Reference.master >>? fun parent ->
    Store.write store empty_tree >>? fun (tree, _) ->
    Store.write store (c0 tree parent) >>? fun (commit, _) ->
    Store.Ref.write store Git.Reference.master (Git.Reference.uid commit)
  in
  let fiber2 () =
    let open Rresult in
    fiber0 ()
    >>= (Lwt_main.run <.> fiber1)
    >>= (R.join
        <.> Bos.OS.Dir.with_current path @@ fun () ->
            Bos.OS.Cmd.run Bos.Cmd.(v "git" % "fsck"))
  in
  match fiber2 () with
  | Ok _ -> Alcotest.(check pass) "git fsck" () ()
  | Error err -> Alcotest.failf "store: %a" Store.pp_error err

open Cmdliner

let store =
  let parser x =
    match Fpath.of_string x with
    | Ok v when Sys.is_directory (Fpath.to_string v) ->
        Rresult.(
          R.reword_error (R.msgf "%a" Store.pp_error) (Lwt_main.run (Store.v v)))
    | Ok v -> Rresult.R.error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let pp ppf store = Fpath.pp ppf (Store.root store) in
  Arg.conv (parser, pp)

let tmp = "tmp"

let () =
  let fiber =
    let open Bos in
    let open Rresult in
    OS.Dir.current () >>= fun current ->
    OS.Dir.create Fpath.(current / tmp) >>= fun _ -> R.ok Fpath.(current / tmp)
  in
  let tmp = Rresult.R.failwith_error_msg fiber in
  Bos.OS.Dir.set_default_tmp tmp;

  let random =
    let create () =
      let open Rresult in
      Bos.OS.Dir.tmp "git-%s" >>= fun root ->
      ( Bos.OS.Dir.with_current root @@ fun () ->
        git_init_with_branch "master" >>= fun () -> R.ok root )
        ()
    in
    match Rresult.(R.join (create ())) with
    | Ok v -> Rresult.R.get_ok (Lwt_main.run (Store.v v))
    | Error err -> Fmt.failwith "%a" Rresult.R.pp_msg err
  in

  let store =
    let doc = "A git repository." in
    Arg.(value & opt store random & info [ "git" ] ~doc)
  in

  Lwt_main.run (Test.test store);

  (* XXX(dinosaure): completely weird... I don't have time to try
     to find a better way to execute common tests and Unix-specific
     tests. TODO! *)
  Alcotest.run_with_args "git-unix" store
    [
      "init", [ git_init ];
      ( "write",
        [
          check_blobs_with_git;
          check_trees_with_git;
          check_commits_with_git;
          check_tags_with_git;
          check_references_with_git;
          empty_commit;
        ] );
      "packed-refs", [ packed_refs ];
      "pack", [ pack_file ];
    ]
