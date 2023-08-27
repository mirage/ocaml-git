module Hash = Git.Hash.Make (Digestif.SHA1)
module Value = Git.Value.Make (Hash)

let () = Random.self_init ()

let random_string len =
  let buf = Buffer.create len in
  for _ = 0 to len - 1 do
    Buffer.add_char buf (Char.chr (Random.int 256))
  done;
  Buffer.contents buf

let value = Alcotest.testable Value.pp Value.equal

let examples : (string * Value.t) list =
  let blob = Value.Blob.of_string (random_string 100) |> Value.blob in
  let tree = Value.Tree.v [] |> Value.tree in
  let author = Git.User.{ name = "me"; email = "me@me.com"; date = 0L, None } in
  let tree_hash = Value.digest tree in
  let long_message = random_string 255 in
  let commit =
    Value.Commit.make ~author ~committer:author ~tree:tree_hash
      (Some long_message)
    |> Value.commit
  in
  let tag =
    Value.Tag.make tree_hash Tree ~tagger:author ~tag:"v1.0" (Some long_message)
    |> Value.tag
  in
  [ "blob", blob; "tree", tree; "commit", commit; "tag", tag ]

let test_to_and_of_raw =
  Alcotest.test_case "to_raw and of_raw_with_header roundtrip" `Quick
  @@ fun () ->
  let check_roundtrip (msg, base) =
    let raw = Value.to_raw base in
    let of_raw = Value.of_raw_with_header raw |> Rresult.R.failwith_error_msg in
    Alcotest.check value msg base of_raw
  in
  List.iter check_roundtrip examples

let test_length_with_header =
  Alcotest.test_case "lenght_with_header" `Quick @@ fun () ->
  let check_length_with_header (msg, v) =
    let raw = Value.to_raw v in
    let raw_length = String.length raw in
    let length_with_header = Value.length_with_header v |> Int64.to_int in
    Alcotest.(check int msg raw_length length_with_header)
  in
  List.iter check_length_with_header examples

let test_of_raw_middle_of_buffer =
  Alcotest.test_case "of_raw when in middle of buffer" `Quick @@ fun () ->
  let check_roundtrip (msg, base) =
    let raw = Value.to_raw base in
    let padded_raw = Fmt.str "hello%sworld" raw in
    let of_raw =
      Value.of_raw_with_header ~off:5 padded_raw |> Rresult.R.failwith_error_msg
    in
    Alcotest.check value msg base of_raw
  in
  List.iter check_roundtrip examples

let () =
  Random.self_init ();
  Alcotest.run "git-value"
    [
      "raw", [ test_to_and_of_raw; test_of_raw_middle_of_buffer ];
      "value", [ test_length_with_header ];
    ]
