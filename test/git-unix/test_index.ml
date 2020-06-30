module Store = Git_unix.Store
module Entry = Git_unix.Index.Entry (Store.Hash)
module Index = Git_unix.Index.Make (Store) (Git_unix.Fs) (Entry)
module Common = Test_common.Make (Store)
open Common

let mkdir ?(perm = 0o755) path = Unix.mkdir (Fpath.to_string path) perm

let touch filename =
  let oc = open_out (Fpath.to_string filename) in
  let () = close_out oc in
  ()

let output_of_command ~root ?env ?input command =
  let old_path = Unix.getcwd () in
  Unix.chdir (Fpath.to_string root) ;
  let res = Test_data.output_of_command ?env ?input command in
  Unix.chdir old_path ; res

let remove path = Unix.unlink (Fpath.to_string path)
let link src dst = Unix.symlink src (Fpath.to_string dst)

let echo_to_file filename content =
  let oc = open_out_bin (Fpath.to_string filename) in
  let () = output_string oc content in
  let () = output_char oc '\n' in
  let () = close_out oc in
  ()

let hash_of_file filename =
  let ic = open_in (Fpath.to_string filename) in
  let by = Bytes.create 0x800 in
  let ctx = Store.Hash.init () in
  let ln = in_channel_length ic in
  let rec go ctx =
    match input ic by 0 0x800 with
    | 0 -> Store.Hash.get ctx
    | len ->
        let ctx = Store.Hash.feed_bytes ctx (Bytes.sub by 0 len) in
        go ctx
  in
  let ctx = Store.Hash.feed_string ctx (Fmt.strf "blob %d\000" ln) in
  let hash = go ctx in
  let () = close_in ic in
  hash

let hash_of_symlink filename =
  let link = Unix.readlink (Fpath.to_string filename) in
  let link = Astring.String.map (function '\\' -> '/' | chr -> chr) link in
  let ctx = Store.Hash.init () in
  let ctx =
    Store.Hash.feed_string ctx
      (Fmt.strf "blob %d\000%s" (String.length link) link)
  in
  Store.Hash.get ctx

let stat path = Unix.stat (Fpath.to_string path)
let lstat path = Unix.lstat (Fpath.to_string path)

(* dbuenzli <3 *)
let parse_value s =
  let open Astring in
  match String.Sub.(head (of_string s)) with
  | Some '"' ->
      let is_data = function '\\' | '"' -> false | _ -> true in
      let rec loop acc s =
        let data, rem = String.Sub.span ~sat:is_data s in
        match String.Sub.head rem with
        | Some '"' ->
            let acc = List.rev (data :: acc) in
            String.Sub.(to_string @@ concat acc), String.Sub.tail rem
        | Some '\\' -> (
            let rem = String.Sub.tail rem in
            match String.Sub.head rem with
            | Some (('"' | '\\') as c) ->
                let acc = String.(sub (of_char c)) :: data :: acc in
                loop acc (String.Sub.tail rem)
            | Some _ | None -> invalid_arg "parse_value" )
        | None | Some _ -> invalid_arg "parse_value"
      in
      loop [] String.Sub.(tail (of_string s))
  | Some _ ->
      let is_data c = not (Char.Ascii.is_white c) in
      let data, rem = String.Sub.span ~sat:is_data (String.Sub.of_string s) in
      String.Sub.to_string data, rem
  | None -> "", String.Sub.of_string s

let ( >>?= ) = Lwt_result.bind

module Git_status = struct
  type t = [`Add of Git.Path.t]

  let pp ppf = function `Add path -> Fmt.pf ppf "A %a" Git.Path.pp path

  let equal a b =
    match a, b with `Add a, `Add b -> Git.Path.equal a b | _, _ -> false

  let compare a b =
    match a, b with `Add a, `Add b -> Git.Path.compare a b | _, _ -> -1

  let of_output output =
    List.fold_left
      (fun acc line ->
        let s, p = Astring.String.span ~sat:(( <> ) ' ') line in
        let v, _ =
          parse_value (Astring.String.trim ~drop:Astring.Char.Ascii.is_white p)
        in
        match String.sub s 0 1, Git.Path.v v with
        | "A", path -> `Add path :: acc
        | _, _ -> acc
        | exception Invalid_argument _ -> acc )
      []
      (Astring.String.cuts ~sep:"\n" output)

  module List = struct
    let pp ppf lst = Fmt.Dump.list pp ppf lst

    let equal a b =
      try List.for_all2 equal (List.sort compare a) (List.sort compare b)
      with _ -> false
  end
end

let is_wsp = function ' ' | '\t' -> true | _ -> false

module Git_ls_files = struct
  type elt = Store.Value.Tree.perm * Store.Hash.t * int * Fpath.t
  type t = elt list

  let of_output output =
    List.fold_left
      (fun acc line ->
        let lst = Astring.String.fields ~is_sep:is_wsp line in
        let lst =
          List.fold_left
            (fun acc -> function "" -> acc | x -> x :: acc)
            [] lst
        in
        let lst = List.rev lst in
        match lst with
        | [perm; hash; n; path] -> (
          try
            let perm = Store.Value.Tree.perm_of_string perm in
            let hash = Store.Hash.of_hex hash in
            let n = int_of_string n in
            let v, _ = parse_value path in
            let path = Fpath.v v in
            (perm, hash, n, path) :: acc
          with _ -> acc )
        | _ -> acc )
      []
      (Astring.String.cuts ~sep:"\n" output)

  let pp ppf lst =
    let pp_elt ppf (perm, hash, n, path) =
      Fmt.pf ppf "%s %a %d %a"
        (Store.Value.Tree.string_of_perm perm)
        Store.Hash.pp hash n Fpath.pp path
    in
    Fmt.Dump.list pp_elt ppf lst

  let compare_elt (_, a, _, _) (_, b, _, _) = Store.Hash.compare a b
  let equal_elt a b = compare_elt a b = 0

  let equal a b =
    List.for_all2 equal_elt (List.sort compare_elt a) (List.sort compare_elt b)
end

module Git_ls_tree = struct
  type kind = [`Blob | `Tree | `Commit | `Tag]
  type elt = Store.Value.Tree.perm * kind * Store.Hash.t * Git.Path.t
  type t = elt list

  let compare_elt (_, _, a, _) (_, _, b, _) = Store.Hash.compare a b
  let equal_elt a b = compare_elt a b = 0

  let equal a b =
    try
      List.for_all2 equal_elt (List.sort compare_elt a)
        (List.sort compare_elt b)
    with _ -> false

  let pp ppf lst =
    let pp_kind : kind Fmt.t =
     fun ppf -> function
      | `Commit -> Fmt.string ppf "commit"
      | `Tag -> Fmt.string ppf "tag"
      | `Tree -> Fmt.string ppf "tree"
      | `Blob -> Fmt.string ppf "blob"
    in
    let pp_elt ppf (perm, kind, hash, path) =
      Fmt.pf ppf "%s %a %a %a"
        (Store.Value.Tree.string_of_perm perm)
        pp_kind kind Store.Hash.pp hash Git.Path.pp path
    in
    Fmt.Dump.list pp_elt ppf lst

  let kind_of_string = function
    | "blob" -> `Blob
    | "tree" -> `Tree
    | "tag" -> `Tag
    | "cmmit" -> `Commit
    | _ -> invalid_arg "kind_of_string"

  let of_output output =
    List.fold_left
      (fun acc line ->
        let lst = Astring.String.fields ~is_sep:is_wsp line in
        let lst =
          List.fold_left
            (fun acc -> function "" -> acc | x -> x :: acc)
            [] lst
        in
        let lst = List.rev lst in
        match lst with
        | [perm; kind; hash; path] -> (
          try
            let perm = Store.Value.Tree.perm_of_string perm in
            let kind = kind_of_string kind in
            let hash = Store.Hash.of_hex hash in
            let path = Git.Path.v path in
            (perm, kind, hash, path) :: acc
          with _ -> acc )
        | _ -> acc )
      []
      (Astring.String.cuts ~sep:"\n" output)
end

let git_status = Alcotest.(testable Git_status.List.pp Git_status.List.equal)
let git_ls_files = Alcotest.(testable Git_ls_files.pp Git_ls_files.equal)
let git_ls_tree = Alcotest.(testable Git_ls_tree.pp Git_ls_tree.equal)

let make_empty_index t fs =
  let dtmp = Cstruct.create 0x800 in
  Index.store_entries t fs ~dtmp []
  >>?= fun () ->
  let command = Fmt.strf "git status --porcelain" in
  let output = output_of_command ~root:(Store.root t) command in
  let status = Git_status.of_output output in
  Alcotest.(check git_status) "empty repository" [] status ;
  Lwt.return (Ok ())

let update_index_add_one_file t fs path =
  let dtmp = Cstruct.create 0x800 in
  Index.load_entries t ~dtmp fs
  >>?= fun entries ->
  let hash = hash_of_file Git.Path.(Store.root t + path) in
  let entry =
    Index.entry_of_stats hash path (stat Git.Path.(Store.root t + path))
  in
  Fmt.epr "> @[%a@].\n%!" Fmt.(Dump.list Entry.pp_entry) entries ;
  Fmt.epr "> @[%a@].\n%!" Entry.pp_entry entry ;
  Index.store_entries t fs ~dtmp (entry :: entries)

let update_index_add_one_symlink t fs path =
  let dtmp = Cstruct.create 0x800 in
  Index.load_entries t ~dtmp fs
  >>?= fun entries ->
  let hash = hash_of_symlink Git.Path.(Store.root t + path) in
  Fmt.(pf stderr) "> hash of %a: %a.\n%!" Git.Path.pp path Store.Hash.pp hash ;
  let entry =
    Index.entry_of_stats hash path (lstat Git.Path.(Store.root t + path))
  in
  Index.store_entries t fs ~dtmp (entry :: entries)

let update_index_remove_one_file t fs path =
  let dtmp = Cstruct.create 0x800 in
  Index.load_entries t ~dtmp fs
  >>?= fun entries ->
  let entries =
    List.filter
      (fun entry -> not (Git.Path.equal path entry.Entry.path))
      entries
  in
  Index.store_entries t fs ~dtmp entries

let run ~root ~pp_error f () =
  let open Lwt.Infix in
  match
    Lwt_main.run
      ( Store.v root
      >>= function
      | Ok t -> f t >>= fun v -> Lwt.return (Ok v)
      | Error err -> Lwt.return (Error err) )
  with
  | Ok (Ok _) -> ()
  | Ok (Error err) -> Alcotest.failf "Retrieve an error: %a" pp_error err
  | Error err -> Alcotest.failf "Retrieve a store error: %a" Store.pp_error err

let root = Fpath.v "test-index"

let test000 root _ : unit Alcotest.test_case =
  Alcotest.test_case (Fmt.strf "make directory %a" Fpath.pp root) `Slow
    (fun () ->
      try mkdir root with exn ->
        Alcotest.failf "Retrieve an error: %s" (Printexc.to_string exn) )

let test001 root fs : unit Alcotest.test_case =
  Alcotest.test_case
    (Fmt.strf "empty index file")
    `Slow
    (run ~root ~pp_error:Index.pp_error (fun t -> make_empty_index t fs))

let test002 root _ : unit Alcotest.test_case =
  Alcotest.test_case "touch should-be-empty" `Slow (fun () ->
      try touch Fpath.(root / "should-be-empty") with exn ->
        Alcotest.failf "Retrieve an error: %s" (Printexc.to_string exn) )

let test003 root fs : unit Alcotest.test_case =
  Alcotest.test_case "git update-index with --add" `Slow
    ( run ~root ~pp_error:Index.pp_error
    @@ fun t ->
    update_index_add_one_file t fs (Git.Path.v "should-be-empty")
    >>?= fun () ->
    let command = Fmt.strf "git status --porcelain" in
    let output = output_of_command ~root:(Store.root t) command in
    let status = Git_status.of_output output in
    Fmt.epr "RECEIVE: @[%a@]." Fmt.(Dump.list Git_status.pp) status ;
    Alcotest.(check git_status)
      "should-be-empty"
      [`Add Git.Path.(v "should-be-empty")]
      status ;
    Lwt.return (Ok ()) )

let test004 root fs : unit Alcotest.test_case =
  Alcotest.test_case "write-tree should-be-empty" `Slow
    ( run ~root ~pp_error:Index.pp_error
    @@ fun t ->
    Index.Write.update_on_store t fs
    >>?= fun hash ->
    assert_key_equal "write-tree" hash
      (Store.Hash.of_hex "7bb943559a305bdd6bdee2cef6e5df2413c3d30a") ;
    Lwt.return (Ok ()) )

let test005 root _ : unit Alcotest.test_case =
  Alcotest.test_case "delete should-be-empty" `Slow (fun () ->
      try remove Fpath.(root / "should-be-empty") with exn ->
        Alcotest.failf "Retrieve an error: %s" (Printexc.to_string exn) )

(* XXX(dinosaure): at this stage, `git` does not check status of repository.
   From what I know, it said: A should-be-empty D should-be-empty

   Which is a very weird state, so I don't test it too. *)

let test006 root fs : unit Alcotest.test_case =
  Alcotest.test_case "git update-index with --remove" `Slow
    ( run ~root ~pp_error:Index.pp_error
    @@ fun t ->
    update_index_remove_one_file t fs (Git.Path.v "should-be-empty")
    >>?= fun () ->
    let command = Fmt.strf "git status --porcelain" in
    let output = output_of_command ~root:(Store.root t) command in
    let status = Git_status.of_output output in
    Alcotest.(check git_status) "empty" [] status ;
    Lwt.return (Ok ()) )

let test007 root fs : unit Alcotest.test_case =
  Alcotest.test_case "write-tree empty" `Slow
    ( run ~root ~pp_error:Index.pp_error
    @@ fun t ->
    Index.Write.update_on_store t fs
    >>?= fun hash ->
    assert_key_equal "write-tree" hash
      (Store.Hash.of_hex "4b825dc642cb6eb9a060e54bf8d69288fbee4904") ;
    Lwt.return (Ok ()) )

let test008 root _ : unit Alcotest.test_case =
  Alcotest.test_case "make directories, files and links" `Slow (fun () ->
      try
        let open Fpath in
        echo_to_file (root / "path0") "hello path0" ;
        mkdir (root / "path2") ;
        mkdir (root / "path3") ;
        mkdir (root / "path3" / "subp3") ;
        echo_to_file (root / "path2" / "file2") "hello path2/file2" ;
        echo_to_file (root / "path3" / "file3") "hello path3/file3" ;
        echo_to_file
          (root / "path3" / "subp3" / "file3")
          "hello path3/subp3/file3" ;
        link "hello path0" (root / "path0sym") ;
        link "hello path2/file2" (root / "path2" / "file2sym") ;
        link "hello path3/file3" (root / "path3" / "file3sym") ;
        link "hello path3/subp3/file3" (root / "path3" / "subp3" / "file3sym")
      with exn ->
        Alcotest.failf "Retrieve an error: %s" (Printexc.to_string exn) )

let test009 root fs : unit Alcotest.test_case =
  Alcotest.test_case "git update-index --add various types of objects" `Slow
    ( run ~root ~pp_error:Index.pp_error
    @@ fun t ->
    let open Git.Path in
    update_index_add_one_file t fs (v "path0")
    >>?= fun () ->
    update_index_add_one_symlink t fs (v "path0sym")
    >>?= fun () ->
    update_index_add_one_file t fs (v "path2" / "file2")
    >>?= fun () ->
    update_index_add_one_symlink t fs (v "path2" / "file2sym")
    >>?= fun () ->
    update_index_add_one_file t fs (v "path3" / "file3")
    >>?= fun () ->
    update_index_add_one_symlink t fs (v "path3" / "file3sym")
    >>?= fun () ->
    update_index_add_one_file t fs (v "path3" / "subp3" / "file3")
    >>?= fun () ->
    update_index_add_one_symlink t fs (v "path3" / "subp3" / "file3sym")
    >>?= fun () ->
    let command = Fmt.strf "git status --porcelain" in
    let output = output_of_command ~root:(Store.root t) command in
    let status = Git_status.of_output output in
    Alcotest.(check git_status)
      "various kind of objects"
      [ `Add (v "path0")
      ; `Add (v "path0sym")
      ; `Add (v "path2" / "file2")
      ; `Add (v "path2" / "file2sym")
      ; `Add (v "path3" / "file3")
      ; `Add (v "path3" / "file3sym")
      ; `Add (v "path3" / "subp3" / "file3")
      ; `Add (v "path3" / "subp3" / "file3sym") ]
      status ;
    Lwt.return (Ok ()) )

let test010 root _ : unit Alcotest.test_case =
  Alcotest.test_case "git ls-files" `Slow
    ( run ~root ~pp_error:(fun _ppf _ -> assert false)
    @@ fun t ->
    let open Fpath in
    let command = Fmt.strf "git ls-files --stage" in
    let output = output_of_command ~root:(Store.root t) command in
    let ls_files = Git_ls_files.of_output output in
    Alcotest.(check git_ls_files)
      "git ls-files --stage"
      [ ( `Normal
        , Store.Hash.of_hex "f87290f8eb2cbbea7857214459a0739927eab154"
        , 0
        , v "path0" )
      ; ( `Link
        , Store.Hash.of_hex "15a98433ae33114b085f3eb3bb03b832b3180a01"
        , 0
        , v "path0sym" )
      ; ( `Normal
        , Store.Hash.of_hex "3feff949ed00a62d9f7af97c15cd8a30595e7ac7"
        , 0
        , v "path2" / "file2" )
      ; ( `Link
        , Store.Hash.of_hex "d8ce161addc5173867a3c3c730924388daedbc38"
        , 0
        , v "path2" / "file2sym" )
      ; ( `Normal
        , Store.Hash.of_hex "0aa34cae68d0878578ad119c86ca2b5ed5b28376"
        , 0
        , v "path3" / "file3" )
      ; ( `Link
        , Store.Hash.of_hex "8599103969b43aff7e430efea79ca4636466794f"
        , 0
        , v "path3" / "file3sym" )
      ; ( `Normal
        , Store.Hash.of_hex "00fb5908cb97c2564a9783c0c64087333b3b464f"
        , 0
        , v "path3" / "subp3" / "file3" )
      ; ( `Link
        , Store.Hash.of_hex "6649a1ebe9e9f1c553b66f5a6e74136a07ccc57c"
        , 0
        , v "path3" / "subp3" / "file3sym" ) ]
      ls_files ;
    Lwt.return (Ok ()) )

let test011 root fs : unit Alcotest.test_case =
  Alcotest.test_case "git write-tree" `Slow
    ( run ~root ~pp_error:Index.pp_error
    @@ fun t ->
    Index.Write.update_on_store t fs
    >>?= fun hash ->
    assert_key_equal "write-tree" hash
      (Store.Hash.of_hex "087704a96baf1c2d1c869a8b084481e121c88b5b") ;
    Lwt.return (Ok ()) )

let test012 root _ : unit Alcotest.test_case =
  Alcotest.test_case "git ls-tree" `Slow
    ( run ~root ~pp_error:Store.pp_error
    @@ fun t ->
    let root_tree =
      Store.Hash.of_hex "087704a96baf1c2d1c869a8b084481e121c88b5b"
    in
    let command = Fmt.strf "git ls-tree %a" Store.Hash.pp root_tree in
    let output = output_of_command ~root:(Store.root t) command in
    let ls_tree = Git_ls_tree.of_output output in
    Store.read t root_tree
    >>?= function
    | Store.Value.Tree tree ->
        let lst =
          List.map
            (function
              | {Store.Value.Tree.perm= `Dir; node; name} ->
                  `Dir, `Tree, node, Git.Path.v name
              | {Store.Value.Tree.perm; node; name} ->
                  perm, `Blob, node, Git.Path.v name)
            (Store.Value.Tree.to_list tree)
        in
        Alcotest.(check git_ls_tree) "git ls-tree" lst ls_tree ;
        Lwt.return (Ok ())
    | value ->
        Alcotest.failf "Expected a tree git object from %a and have: %a"
          Store.Hash.pp root_tree Store.Value.pp value )

let test013 root _ : unit Alcotest.test_case =
  Alcotest.test_case "git ls-tree recursive" `Slow
    ( run ~root ~pp_error:Store.pp_error
    @@ fun t ->
    let open Lwt.Infix in
    let root_tree =
      Store.Hash.of_hex "087704a96baf1c2d1c869a8b084481e121c88b5b"
    in
    let command = Fmt.strf "git ls-tree -r %a" Store.Hash.pp root_tree in
    let output = output_of_command ~root:(Store.root t) command in
    let ls_tree = Git_ls_tree.of_output output in
    let perms = Hashtbl.create 128 in
    Store.fold t
      (fun acc ?name ~length:_ hash value ->
        match name, value with
        | Some path, Store.Value.Tree tree ->
            let () =
              List.iter
                (fun {Store.Value.Tree.perm; name; _} ->
                  Hashtbl.add perms Git.Path.(path / name) perm )
                (Store.Value.Tree.to_list tree)
            in
            Lwt.return acc
        | Some path, Store.Value.Blob _ ->
            Lwt.return ((Hashtbl.find perms path, `Blob, hash, path) :: acc)
        | Some path, Store.Value.Commit _ ->
            Lwt.return ((Hashtbl.find perms path, `Commit, hash, path) :: acc)
        | Some path, Store.Value.Tag _ ->
            Lwt.return ((Hashtbl.find perms path, `Tag, hash, path) :: acc)
        | None, _ -> Alcotest.failf "Expected only named git object" )
      ~path:Git.Path.empty [] root_tree
    >>= fun lst ->
    Alcotest.(check git_ls_tree) "git ls-tree -r" lst ls_tree ;
    Lwt.return (Ok ()) )

let test014 root _ : unit Alcotest.test_case =
  Alcotest.test_case "git ls-tree recursive with tree" `Slow
    ( run ~root ~pp_error:Store.pp_error
    @@ fun t ->
    let open Lwt.Infix in
    let root_tree =
      Store.Hash.of_hex "087704a96baf1c2d1c869a8b084481e121c88b5b"
    in
    let command = Fmt.strf "git ls-tree -r -t %a" Store.Hash.pp root_tree in
    let output = output_of_command ~root:(Store.root t) command in
    let ls_tree = Git_ls_tree.of_output output in
    let perms = Hashtbl.create 128 in
    Store.fold t
      (fun acc ?name ~length:_ hash value ->
        match name, value with
        | Some path, Store.Value.Tree tree ->
            let () =
              List.iter
                (fun {Store.Value.Tree.perm; name; _} ->
                  Hashtbl.add perms Git.Path.(path / name) perm )
                (Store.Value.Tree.to_list tree)
            in
            Lwt.return ((`Dir, `Tree, hash, path) :: acc)
        | Some path, Store.Value.Blob _ ->
            Lwt.return ((Hashtbl.find perms path, `Blob, hash, path) :: acc)
        | Some path, Store.Value.Commit _ ->
            Lwt.return ((Hashtbl.find perms path, `Commit, hash, path) :: acc)
        | Some path, Store.Value.Tag _ ->
            Lwt.return ((Hashtbl.find perms path, `Tag, hash, path) :: acc)
        | None, _ -> Alcotest.failf "Expected only named git object" )
      ~path:Git.Path.empty [] root_tree
    >>= fun lst ->
    Alcotest.(check git_ls_tree)
      "git ls-tree -r" lst
      ((`Dir, `Tree, root_tree, Git.Path.root) :: ls_tree) ;
    Lwt.return (Ok ()) )

let test015 root fs : unit Alcotest.test_case =
  Alcotest.test_case "git read-tree" `Slow
    ( run ~root ~pp_error:Index.pp_error
    @@ fun t ->
    let open Lwt.Infix in
    let ( >>?= ) = Lwt_result.bind in
    let () = Unix.unlink Fpath.(to_string (Store.dotgit t / "index")) in
    let dtmp = Cstruct.create 0x800 in
    let root = Store.Hash.of_hex "087704a96baf1c2d1c869a8b084481e121c88b5b" in
    Index.Read.of_tree t root
    >>= fun entries ->
    Fmt.(pf stderr) "We will write: %a.\n" Entry.pp_index entries ;
    Index.store_entries t fs ~dtmp entries
    >>?= fun () ->
    Index.Write.update_on_store t fs
    >>?= fun hash ->
    assert_key_equal "write-tree" hash root ;
    Lwt.return (Ok ()) )

let suite root =
  ( "index"
  , List.map
      (fun f -> f root ())
      [ test000; test001; test002; test003; test004; test005; test006; test007
      ; test008; test009; test010; test011; test012; test013; test014; test015
      ] )
