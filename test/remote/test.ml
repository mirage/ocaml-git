(* End-to-end tests for [Git.Remote.push_new_file] against a real [git]: every
   case below talks to [git-upload-pack] and [git-receive-pack] over a pair of
   named pipes, exactly like the [ssh] cases of [test/smart]. Nothing is
   simulated but the transport, and every assertion is made by asking the real
   [git] binary what ended up in the remote repository. *)

open Bos

let () = Printexc.record_backtrace true
let () = Mirage_crypto_rng_unix.use_default ()

(* The transport, mirroring [test/smart/fifo.ml]: a pair of named pipes per
   connection. [push_new_file] connects twice - once to fetch, once to push -
   and each connection takes the next pair, because reusing a single pair makes
   the second [open] race the tear-down of the first session: it can land on
   the dying writer and read an immediate end of input. *)
module Fifo = struct
  open Lwt.Infix

  type flow = {
    ic : Lwt_unix.file_descr;
    oc : Lwt_unix.file_descr;
    linger : Bytes.t;
  }

  type endpoint = (Fpath.t * Fpath.t) list ref
  type error = |
  type write_error = [ `Closed | `Exhausted ]

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  let pp_write_error ppf = function
    | `Closed -> Fmt.string ppf "Closed by peer"
    | `Exhausted -> Fmt.string ppf "No named pipe left"
  let io_buffer_size = 65536

  let connect pipes =
    match !pipes with
    | [] -> Lwt.return_error `Exhausted
    | (ic, oc) :: rest ->
        pipes := rest;
        Lwt_unix.openfile (Fpath.to_string ic) Unix.[ O_RDONLY ] 0o600
        >>= fun ic ->
        Lwt_unix.openfile (Fpath.to_string oc) Unix.[ O_WRONLY ] 0o600
        >>= fun oc ->
        Lwt.return_ok { ic; oc; linger = Bytes.create io_buffer_size }

  let read { ic; linger; _ } :
      (Cstruct.t Mirage_flow.or_eof, error) result Lwt.t =
    Lwt_unix.read ic linger 0 (Bytes.length linger) >>= function
    | 0 -> Lwt.return_ok `Eof
    | len -> Lwt.return_ok (`Data (Cstruct.of_bytes linger ~off:0 ~len))

  let write { oc; _ } cs : (unit, write_error) result Lwt.t =
    let rec go ({ Cstruct.buffer; off; len } as cs) =
      if len = 0 then Lwt.return_ok ()
      else
        Lwt_bytes.write oc buffer off len >>= fun len ->
        go (Cstruct.shift cs len)
    in
    go cs

  let writev t css =
    List.fold_left
      (fun acc cs -> Lwt_result.bind acc (fun () -> write t cs))
      (Lwt.return_ok ()) css

  let close t = Lwt_unix.close t.ic >>= fun () -> Lwt_unix.close t.oc
  let shutdown t _ = close t
end

let fifo_value, _fifo = Mimic.register ~name:"fifo" (module Fifo)

let ctx_with_fifo pipes =
  Mimic.empty
  |> Mimic.add fifo_value (ref pipes)
  |> Mimic.add Smart_git.git_transmission `Exec

let p = Fpath.to_string
let failed out = String.starts_with ~prefix:"!" out
let error_to_string (`Msg err) = "!" ^ err

(* Real [git], driven through [bos]. The output is returned as a string even
   when the command fails, so that a broken set-up shows up inside the
   assertion which needed it instead of silently making the test vacuous. *)
let git repo args () =
  OS.Cmd.run_out ~err:OS.Cmd.err_run_out
    Cmd.(v "git" % "-C" % p repo %% of_list args)
  |> OS.Cmd.out_string ~trim:true
  |> Result.fold ~error:error_to_string ~ok:(fun (out, (info, status)) ->
         match status with
         | `Exited 0 -> out
         | `Exited _ | `Signaled _ ->
             Fmt.str "!%a: %a: %s" Cmd.pp (OS.Cmd.run_info_cmd info)
               OS.Cmd.pp_status status out)

(* The steps of a set-up have to run in the order they are written: the order
   in which OCaml evaluates the elements of a list is not specified. *)
let in_order steps =
  List.fold_left (fun acc step -> List.append acc [ step () ]) [] steps

(* [git-upload-pack] then [git-receive-pack], in the order in which
   [push_new_file] connects: the fetch first, the push second. A single child
   runs them one after the other, each on its own pair of pipes. *)
let fork_git_services repo services =
  let ( let* ) = Result.bind in
  let serve (service, (ic, oc)) =
    let* cat = OS.Cmd.run_out Cmd.(v "cat" % p oc) |> OS.Cmd.out_run_in in
    let* git = OS.Cmd.run_io Cmd.(v service % p repo) cat |> OS.Cmd.out_run_in in
    (* [tee] also copies the wire protocol to its own standard output, which
       would end up in the middle of the test report: drop it. *)
    OS.Cmd.run_io Cmd.(v "tee" % p ic) git |> OS.Cmd.to_null
  in
  match Unix.fork () with
  | 0 ->
      (* Its own session, hence its own process group, so that
         [stop_git_services] can take [cat], [git-*] and [tee] down with it. *)
      let (_ : int) = Unix.setsid () in
      let outcome =
        List.fold_left
          (fun acc service -> Result.bind acc (fun () -> serve service))
          (Ok ()) services
      in
      Unix._exit
        (Result.fold ~ok:(fun () -> 0) ~error:(fun (`Msg _) -> 1) outcome)
  | pid -> pid

(* A case which stops at [`Conflict] never opens the second connection, so the
   services would wait on their pipes forever. They are killed rather than
   waited for, then reaped so that no zombie is left behind. *)
let stop_git_services pid =
  (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
  (* [Lwt_unix.waitpid], not [Unix.waitpid]: Lwt's own SIGCHLD handler
     interrupts the blocking one with [EINTR]. *)
  Lwt_unix.waitpid [] pid
  |> Lwt.map (fun (_ : int * Unix.process_status) -> ())

let mkfifo path =
  Unix.mkfifo (Fpath.to_string path) 0o600;
  path

let write_seed work (path, contents) () =
  let file = Fpath.(work // v path) in
  OS.Dir.create (Fpath.parent file)
  |> (fun created ->
       Result.bind created (fun (_ : bool) -> OS.File.write file contents))
  |> Result.fold ~ok:(fun () -> "") ~error:error_to_string

(* A bare remote repository holding [seed], plus the pipes and the [ctx] which
   [push_new_file] will use to reach it. An empty [seed] means a repository
   with no reference at all, which is a case of its own. *)
let remote_repository ~seed root =
  let remote = Fpath.add_seg root "remote.git" in
  let work = Fpath.add_seg root "work" in
  let create =
    in_order
      [
        git root [ "init"; "-q"; "--bare"; p remote ];
        (* [git init --bare] points HEAD at the default branch of whoever runs
           the tests, which is not necessarily the branch seeded below. *)
        git remote [ "symbolic-ref"; "HEAD"; "refs/heads/master" ];
      ]
  in
  let populate =
    match seed with
    | [] -> []
    | _ :: _ ->
        in_order
          (List.concat
             [
               [ git root [ "init"; "-q"; p work ] ];
               List.map (write_seed work) seed;
               [
                 git work [ "config"; "user.name"; "test" ];
                 git work [ "config"; "user.email"; "test@nowhere.invalid" ];
                 git work [ "add"; "-A" ];
                 git work [ "commit"; "-q"; "-m"; "seed" ];
                 git work [ "push"; "-q"; p remote; "HEAD:refs/heads/master" ];
               ];
             ])
  in
  let services =
    List.mapi
      (fun n service ->
        ( service,
          ( mkfifo (Fpath.add_seg root (Fmt.str "ic-%d" n)),
            mkfifo (Fpath.add_seg root (Fmt.str "oc-%d" n)) ) ))
      [ "git-upload-pack"; "git-receive-pack" ]
  in
  let pid = fork_git_services remote services in
  ( remote,
    ctx_with_fifo (List.map snd services),
    pid,
    List.filter failed (List.append create populate) )

let endpoint =
  Smart_git.Endpoint.of_string "git@localhost:remote.git" |> Result.to_option

(* Every outcome of [push_new_file] is rendered as a string so that the checks
   below are plain string comparisons: a check which fails then prints what
   actually happened. *)
let outcome =
  Result.fold
    ~ok:(fun () -> "ok")
    ~error:(function `Conflict -> "conflict" | `Msg err -> "msg: " ^ err)

let author =
  { Git.User.name = "test"; email = "test@nowhere.invalid"; date = (0L, None) }

let push ~ctx ?branch ?author ?message ~path ~contents () =
  Option.fold
    ~none:(fun () -> Lwt.return "msg: unparsable endpoint")
    ~some:(fun edn () ->
      Git.Remote.push_new_file ?branch ?author ?message ~ctx edn
        ~path:(Fpath.v path) ~contents
      |> Lwt.map outcome)
    endpoint ()

(* Runs [f] against a fresh remote seeded with [seed], and always reaps the
   child which serves it. *)
let with_remote ~seed f =
  Option.fold
    ~none:(fun () -> Alcotest.fail "cannot create a temporary directory")
    ~some:(fun root () ->
      let remote, ctx, pid, broken = remote_repository ~seed root in
      Alcotest.(check (list string)) "set-up" [] broken;
      Lwt.finalize
        (fun () -> f ~remote ~ctx)
        (fun () -> stop_git_services pid))
    (OS.Dir.tmp "ocaml-git-remote-%s" |> Result.to_option)
    ()

let case name f = Alcotest_lwt.test_case name `Quick (fun _sw () -> f ())

let test_empty_repository =
  case "push to an empty repository" @@ fun () ->
  with_remote ~seed:[] @@ fun ~remote ~ctx ->
  push ~ctx ~path:"hello.txt" ~contents:"hello\n" ()
  |> Lwt.map @@ fun result ->
  Alcotest.(check string) "push" "ok" result;
  Alcotest.(check string)
    "contents" "hello"
    (git remote [ "cat-file"; "-p"; "refs/heads/master:hello.txt" ] ());
  Alcotest.(check string)
    "the commit is a root" ""
    (git remote [ "log"; "-1"; "--format=%P"; "refs/heads/master" ] ());
  (* No [?author] was given above, so this is the documented default. *)
  Alcotest.(check string)
    "default author" "ocaml-git <ocaml-git@nowhere.invalid> 0"
    (git remote [ "log"; "-1"; "--format=%an <%ae> %at"; "refs/heads/master" ] ())

let test_fresh_directories =
  case "push a file into brand new directories" @@ fun () ->
  with_remote ~seed:[ ("file1.txt", "v1\n") ] @@ fun ~remote ~ctx ->
  push ~ctx ~path:"x/y/added.txt" ~contents:"fresh\n" ()
  |> Lwt.map @@ fun result ->
  Alcotest.(check string) "push" "ok" result;
  Alcotest.(check string)
    "contents" "fresh"
    (git remote [ "cat-file"; "-p"; "refs/heads/master:x/y/added.txt" ] ());
  Alcotest.(check string)
    "the seed survived" "v1"
    (git remote [ "cat-file"; "-p"; "refs/heads/master:file1.txt" ] ());
  Alcotest.(check string)
    "the new commit has a parent" "2"
    (git remote [ "rev-list"; "--count"; "refs/heads/master" ] ());
  (* No [?message] was given above, so this is the documented default. *)
  Alcotest.(check string)
    "default message" "Add x/y/added.txt"
    (git remote [ "log"; "-1"; "--format=%s"; "refs/heads/master" ] ())

let test_existing_directory =
  case "push a file into an existing directory" @@ fun () ->
  with_remote ~seed:[ ("a/b/deep.txt", "deep\n") ] @@ fun ~remote ~ctx ->
  push ~ctx ~path:"a/b/added.txt" ~contents:"beside\n" ()
  |> Lwt.map @@ fun result ->
  Alcotest.(check string) "push" "ok" result;
  Alcotest.(check string)
    "contents" "beside"
    (git remote [ "cat-file"; "-p"; "refs/heads/master:a/b/added.txt" ] ());
  Alcotest.(check string)
    "the neighbour survived" "deep"
    (git remote [ "cat-file"; "-p"; "refs/heads/master:a/b/deep.txt" ] ());
  (* Reading the file back is not enough: a rewritten tree which kept its old
     entry alongside the new one still resolves. [git fsck] is what rejects a
     tree holding two entries of the same name. *)
  Alcotest.(check string)
    "the pushed objects are well formed" ""
    (git remote [ "fsck"; "--no-progress"; "--no-dangling" ] ())

let test_conflict_file =
  case "conflict: the file already exists" @@ fun () ->
  with_remote ~seed:[ ("file1.txt", "v1\n") ] @@ fun ~remote ~ctx ->
  push ~ctx ~path:"file1.txt" ~contents:"overwritten\n" ()
  |> Lwt.map @@ fun result ->
  Alcotest.(check string) "push" "conflict" result;
  Alcotest.(check string)
    "nothing was replaced" "v1"
    (git remote [ "cat-file"; "-p"; "refs/heads/master:file1.txt" ] ());
  Alcotest.(check string)
    "nothing was committed" "1"
    (git remote [ "rev-list"; "--count"; "refs/heads/master" ] ())

let test_conflict_directory =
  case "conflict: a directory is in the way" @@ fun () ->
  with_remote ~seed:[ ("a/b/deep.txt", "deep\n") ] @@ fun ~remote ~ctx ->
  push ~ctx ~path:"a" ~contents:"clash\n" () |> Lwt.map @@ fun result ->
  Alcotest.(check string) "push" "conflict" result;
  Alcotest.(check string)
    "nothing was committed" "1"
    (git remote [ "rev-list"; "--count"; "refs/heads/master" ] ())

let test_conflict_blocking_file =
  case "conflict: a file blocks a directory" @@ fun () ->
  with_remote ~seed:[ ("file1.txt", "v1\n") ] @@ fun ~remote ~ctx ->
  push ~ctx ~path:"file1.txt/nested.txt" ~contents:"clash\n" ()
  |> Lwt.map @@ fun result ->
  Alcotest.(check string) "push" "conflict" result;
  Alcotest.(check string)
    "nothing was committed" "1"
    (git remote [ "rev-list"; "--count"; "refs/heads/master" ] ())

let test_branch_and_message =
  case "branch, author and message" @@ fun () ->
  with_remote ~seed:[ ("file1.txt", "v1\n") ] @@ fun ~remote ~ctx ->
  push ~ctx
    ~branch:(Git.Reference.v "refs/heads/other")
    ~author ~message:"a message" ~path:"only-there.txt" ~contents:"other\n" ()
  |> Lwt.map @@ fun result ->
  Alcotest.(check string) "push" "ok" result;
  Alcotest.(check string)
    "contents" "other"
    (git remote [ "cat-file"; "-p"; "refs/heads/other:only-there.txt" ] ());
  Alcotest.(check string)
    "message" "a message"
    (git remote [ "log"; "-1"; "--format=%s"; "refs/heads/other" ] ());
  Alcotest.(check string)
    "author" "test <test@nowhere.invalid>"
    (git remote [ "log"; "-1"; "--format=%an <%ae>"; "refs/heads/other" ] ());
  Alcotest.(check string)
    "the new branch is a root" ""
    (git remote [ "log"; "-1"; "--format=%P"; "refs/heads/other" ] ());
  Alcotest.(check string)
    "master was left alone" "1"
    (git remote [ "rev-list"; "--count"; "refs/heads/master" ] ())

(* These never reach the network: the path is rejected before any connection is
   opened, which is why they can run against an empty [ctx]. *)
let test_invalid_paths =
  case "invalid paths are rejected before connecting" @@ fun () ->
  let rejected path = push ~ctx:Mimic.empty ~path ~contents:"x" () in
  Lwt_list.map_s rejected
    [ "/absolute.txt"; "../escape.txt"; "a/../b.txt"; "."; "a/" ]
  |> Lwt.map @@ fun results ->
  (* The exact message matters: an empty [ctx] makes {i every} outcome a
     [`Msg], so a check for "some error" would pass even if the path had been
     accepted and the failure came from the transport instead. *)
  Alcotest.(check (list string))
    "rejected"
    [
      "msg: /absolute.txt: not a relative path";
      "msg: ../escape.txt: invalid path segment";
      "msg: a/../b.txt: invalid path segment";
      "msg: .: invalid path segment";
      "msg: a/: invalid path segment";
    ]
    results

let () =
  Lwt_main.run
    (Alcotest_lwt.run "remote"
       [
         ( "push_new_file",
           [
             test_empty_repository;
             test_fresh_directories;
             test_existing_directory;
             test_conflict_file;
             test_conflict_directory;
             test_conflict_blocking_file;
             test_branch_and_message;
             test_invalid_paths;
           ] );
       ])
