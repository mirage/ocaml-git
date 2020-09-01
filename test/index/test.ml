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

open Git_index

let empty =
  Alcotest.test_case "empty" `Quick @@ fun path ->
  let index = make SHA1 in
  let run path =
    let open Rresult in
    Bos.OS.Dir.with_current path @@ fun () ->
    store_to_path ~hash:SHA1 Fpath.(path / ".git" / "index") index >>= fun () ->
    let status = Bos.Cmd.(v "git" % "status" % "--porcelain") in
    let status = Bos.OS.Cmd.run_out status in
    Bos.OS.Cmd.out_lines status >>= function
    | [], _ -> load ~hash:SHA1 Fpath.(path / ".git" / "index")
    | res, _ ->
        Alcotest.failf "git-status: @[<hov>%a@]" Fmt.(Dump.list string) res
  in
  match Rresult.R.join (run path ()) with
  | Ok _t -> ()
  | Error (`Msg err) -> Alcotest.fail err

let add_should_be_empty =
  Alcotest.test_case "should-be-empty" `Quick @@ fun path ->
  let run path =
    let open Rresult in
    Bos.OS.Dir.with_current path @@ fun () ->
    Bos.OS.File.write Fpath.(path / "should-be-empty") "" >>= fun () ->
    let index = make SHA1 in
    add ~hash:SHA1 Fpath.(v "should-be-empty") index >>= fun () ->
    store_to_path ~hash:SHA1 Fpath.(path / ".git" / "index") index >>= fun () ->
    let status = Bos.Cmd.(v "git" % "status" % "--porcelain") in
    let status = Bos.OS.Cmd.run_out status in
    Bos.OS.Cmd.out_lines ~trim:true status >>= function
    | [ should_be_empty ], _ -> (
        match Astring.String.fields ~empty:false should_be_empty with
        | [ status; _ ] ->
            Alcotest.(check string) "add" "A" status;
            R.ok ()
        | _ ->
            Alcotest.failf "git-status: impossible to parse %S" should_be_empty
        )
    | res, _ ->
        Alcotest.failf "git-status: @[<hov>%a@]" Fmt.(Dump.list string) res
  in
  match Rresult.R.join (run path ()) with
  | Ok () -> ()
  | Error (`Msg err) -> Alcotest.fail err

(* write-tree *)

module Search = Git.Search.Make (Digestif.SHA1) (Git_unix.Store)

let ( <.> ) f g x = f (g x)

let ( >>? ) x f =
  let open Lwt.Infix in
  x >>= function Ok x -> f x | Error err -> Lwt.return_error err

let blob_of_path path =
  if (Unix.lstat (Fpath.to_string path)).Unix.st_kind = Unix.S_LNK then
    let contents = Unix.readlink (Fpath.to_string path) in
    let contents =
      Astring.String.map (function '\\' -> '/' | c -> c) contents
    in
    Git.Blob.of_string contents
  else
    let ic = open_in (Fpath.to_string path) in
    let ln = in_channel_length ic in
    let rs = Bytes.create ln in
    really_input ic rs 0 ln;
    Git.Blob.of_string (Bytes.unsafe_to_string rs)

let perm_of_mode entry =
  match Entry.mode entry with
  | 0o120000 -> `Link
  | 0o160000 -> `Commit
  | mode ->
      if mode land 0x8000 <> 0 then
        match mode land 0x7FFF with
        | 0o755 -> `Exec
        | 0o600 | 0o644 -> `Normal
        | _ -> Fmt.invalid_arg "Unknown file mode %o" mode
      else Fmt.invalid_arg "Unknown mode %o" mode

let tree_of_children tbl children =
  let open Lwt.Infix in
  let entry = function
    | `Blob entry ->
        let name = Fpath.basename (Entry.path entry) in
        Lwt.return_ok
          (Git.Tree.entry ~name (perm_of_mode entry) (Entry.oid entry))
    | `Tree path -> (
        match Hashtbl.find_opt tbl path with
        | Some hash ->
            let name = Fpath.basename path in
            Lwt.return_ok (Git.Tree.entry ~name `Dir hash)
        | None -> Alcotest.failf "%a does not exist" Fpath.pp path )
  in
  Lwt_list.map_p entry children
  >>= Lwt_list.map_s (Lwt.return <.> Rresult.R.get_ok)
  >>= fun entries ->
  let tree = Git.Tree.v entries in
  Lwt.return tree

let sha1 = Alcotest.testable Digestif.SHA1.pp Digestif.SHA1.equal

let write_tree expect =
  Alcotest.test_case "write-tree" `Quick @@ fun path ->
  let run path =
    let open Rresult in
    Bos.OS.Dir.with_current path @@ fun () ->
    load ~hash:SHA1 Fpath.(path / ".git" / "index") >>= fun t ->
    let fiber =
      let open Lwt.Infix in
      Git_unix.Store.v path
      >|= Rresult.R.reword_error (Rresult.R.msgf "%a" Git_unix.Store.pp_error)
      >>? fun store ->
      let tbl = Hashtbl.create 0x100 in
      let f v children _ =
        match v with
        | `Blob entry -> (
            let blob = blob_of_path (Entry.path entry) in
            Git_unix.Store.write store (Git_unix.Store.Value.blob blob)
            >>= function
            | Ok (hash, _) ->
                Alcotest.(check sha1) "blob" hash (Entry.oid entry);
                Lwt.return_ok hash
            | Error err ->
                Alcotest.failf "store: %a" Git_unix.Store.pp_error err )
        | `Tree path -> (
            tree_of_children tbl children >>= fun tree ->
            Git_unix.Store.write store (Git_unix.Store.Value.tree tree)
            >>= function
            | Ok (hash, _) ->
                Hashtbl.add tbl path hash;
                Lwt.return_ok hash
            | Error err ->
                Alcotest.failf "store: %a" Git_unix.Store.pp_error err )
        | `Root -> (
            tree_of_children tbl children >>= fun tree ->
            Git_unix.Store.write store (Git_unix.Store.Value.tree tree)
            >>= function
            | Ok (hash, _) -> Lwt.return_ok hash
            | Error err ->
                Alcotest.failf "store: %a" Git_unix.Store.pp_error err )
      in
      fold ~f (Digestif.SHA1.digest_string "") t
    in
    Lwt_main.run fiber >>= fun hash ->
    let cmd = Bos.Cmd.(v "git" % "write-tree") in
    let cmd = Bos.OS.Cmd.run_out cmd in
    Bos.OS.Cmd.out_string ~trim:true cmd >>= fun (hash', _) ->
    let hash' = Digestif.SHA1.of_hex hash' in
    Alcotest.(check sha1) "write-tree (git)" hash hash';
    R.ok hash'
  in
  match Rresult.R.join (run path ()) with
  | Ok root_tree -> Alcotest.(check sha1) "write-tree" root_tree expect
  | Error (`Msg err) -> Alcotest.fail err

let delete_should_be_empty =
  Alcotest.test_case "delete" `Quick @@ fun path ->
  let run path =
    let open Rresult in
    Bos.OS.Dir.with_current path @@ fun () ->
    Bos.OS.Cmd.run Bos.Cmd.(v "git" % "commit" % "-m" % ".") >>= fun () ->
    load ~hash:SHA1 Fpath.(path / ".git" / "index") >>= fun t ->
    (* XXX(dinosaure): [git] deletes [should-be-empty] into the index file **AND**
       concretely into the file-system. *)
    rem Fpath.(v "should-be-empty") t;
    Bos.OS.File.delete Fpath.(path / "should-be-empty") >>= fun () ->
    store_to_path ~hash:SHA1 Fpath.(path / ".git" / "index") t >>= fun () ->
    let cmd = Bos.Cmd.(v "git" % "status" % "--porcelain") in
    let cmd = Bos.OS.Cmd.run_out cmd in
    Bos.OS.Cmd.out_lines ~trim:true cmd >>= function
    | [ should_be_empty ], _ -> (
        match Astring.String.fields ~empty:false should_be_empty with
        | [ status; _ ] ->
            Alcotest.(check string) "delete" status "D";
            Bos.OS.Cmd.run Bos.Cmd.(v "git" % "commit" % "-m" % ".")
        | _ ->
            Alcotest.failf "git-status: impossible to parse %S" should_be_empty
        )
    | res, _ ->
        Alcotest.failf "git-status: @[<hov>%a@]" Fmt.(Dump.list string) res
  in
  match Rresult.R.join (run path ()) with
  | Ok () -> ()
  | Error (`Msg err) -> Alcotest.fail err

let link ~target src =
  try
    Unix.symlink src (Fpath.to_string target);
    Ok ()
  with Unix.Unix_error (err, _, _) ->
    Rresult.R.error_msgf "symlink target:%a %s: %s" Fpath.pp target src
      (Unix.error_message err)

let populate =
  Alcotest.test_case "populate" `Quick @@ fun path ->
  let run_store path =
    Git_unix.Store.v path >>? fun store ->
    Git_unix.Store.write store
      (Git_unix.Store.Value.blob (blob_of_path Fpath.(v "path0")))
    >>? fun _ ->
    Git_unix.Store.write store
      (Git_unix.Store.Value.blob (blob_of_path Fpath.(v "path2/file2")))
    >>? fun _ ->
    Git_unix.Store.write store
      (Git_unix.Store.Value.blob (blob_of_path Fpath.(v "path3/file3")))
    >>? fun _ ->
    Git_unix.Store.write store
      (Git_unix.Store.Value.blob (blob_of_path Fpath.(v "path3/subp3/file3")))
    >>? fun _ -> Lwt.return_ok ()
  in
  let run path =
    let open Rresult in
    Bos.OS.Dir.with_current path @@ fun () ->
    Bos.OS.File.write Fpath.(v "path0") "hello path0" >>= fun () ->
    Bos.OS.Dir.create Fpath.(v "path2") >>= fun _ ->
    Bos.OS.Dir.create Fpath.(v "path3" / "subp3") >>= fun _ ->
    Bos.OS.File.write Fpath.(v "path2" / "file2") "hello path2/file2"
    >>= fun () ->
    Bos.OS.File.write Fpath.(v "path3" / "file3") "hello path3/file3"
    >>= fun () ->
    Bos.OS.File.write
      Fpath.(v "path3" / "subp3" / "file3")
      "hello path3/subp3/file3"
    >>= fun () ->
    link "hello path0" ~target:Fpath.(v "path0sym") >>= fun () ->
    link "hello path2/file2" ~target:Fpath.(v "path2" / "file2sym")
    >>= fun () ->
    link "hello path3/file3" ~target:Fpath.(v "path3" / "file3sym")
    >>= fun () ->
    link "hello path3/subp3/file3"
      ~target:Fpath.(v "path3" / "subp3" / "file3sym")
    >>= fun () ->
    let index = make SHA1 in
    add ~hash:SHA1 Fpath.(v "path0") index >>= fun () ->
    add ~hash:SHA1 Fpath.(v "path2" / "file2") index >>= fun () ->
    add ~hash:SHA1 Fpath.(v "path3" / "file3") index >>= fun () ->
    add ~hash:SHA1 Fpath.(v "path3" / "subp3" / "file3") index >>= fun () ->
    add ~hash:SHA1 Fpath.(v "path0sym") index >>= fun () ->
    add ~hash:SHA1 Fpath.(v "path2" / "file2sym") index >>= fun () ->
    add ~hash:SHA1 Fpath.(v "path3" / "file3sym") index >>= fun () ->
    add ~hash:SHA1 Fpath.(v "path3" / "subp3" / "file3sym") index >>= fun () ->
    Lwt_main.run (run_store path) |> R.reword_error (fun err -> `Store err)
    >>= fun () ->
    store_to_path ~hash:SHA1 Fpath.(path / ".git" / "index") index >>= fun () ->
    let status = Bos.Cmd.(v "git" % "status" % "--porcelain") in
    let status = Bos.OS.Cmd.run_out status in
    Bos.OS.Cmd.out_lines ~trim:true status >>= fun (lst, _) ->
    let lst = List.map (Astring.String.fields ~empty:false) lst in
    let adds = List.map List.hd lst in
    let filenames = List.map (List.hd <.> List.tl) lst in
    Alcotest.(check bool) "only adds" (List.for_all (( = ) "A") adds) true;
    Alcotest.(check (list string))
      "filenames" filenames
      [
        "path0"; "path0sym"; "path2/file2"; "path2/file2sym"; "path3/file3";
        "path3/file3sym"; "path3/subp3/file3"; "path3/subp3/file3sym";
      ];
    R.ok ()
  in
  match Rresult.R.join (run path ()) with
  | Ok () -> ()
  | Error (`Store err) -> Alcotest.failf "git: %a" Git_unix.Store.pp_error err
  | Error (`Msg err) -> Alcotest.fail err

open Cmdliner

let store =
  let parser x =
    match Fpath.of_string x with
    | Ok v when Sys.is_directory (Fpath.to_string v) -> Ok v
    | Ok v -> Rresult.R.error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let pp ppf store = Fpath.pp ppf store in
  Arg.conv (parser, pp)

let random =
  let create () =
    let open Rresult in
    Bos.OS.Dir.tmp "git-%s" >>= fun root ->
    Bos.OS.Dir.with_current root
      (fun () ->
        Bos.OS.Cmd.run Bos.Cmd.(v "git" % "init") >>= fun () -> R.ok root)
      ()
  in
  match Rresult.(R.join (create ())) with
  | Ok v -> v
  | Error err -> Fmt.failwith "%a" Rresult.R.pp_msg err

let store =
  let doc = "A git repository." in
  Arg.(value & opt store random & info [ "git" ] ~doc)

let () =
  Alcotest.run_with_args "index" store
    [
      ( "index",
        [
          empty; add_should_be_empty;
          write_tree
            (Digestif.SHA1.of_hex "7bb943559a305bdd6bdee2cef6e5df2413c3d30a");
          delete_should_be_empty;
          write_tree
            (Digestif.SHA1.of_hex "4b825dc642cb6eb9a060e54bf8d69288fbee4904");
          populate;
          write_tree
            (Digestif.SHA1.of_hex "5392d758224ad8d7c1798cb67b05b24685d5adc4");
        ] );
    ]
