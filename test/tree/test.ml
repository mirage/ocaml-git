external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="

let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done ;
  res

let () =
  let random_seed = seed in
  Fmt.pr "Random: %a.\n%!" Fmt.(Dump.array int) random_seed ;
  Random.full_init random_seed

let random_string len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i (Char.chr (Random.int 256))
  done ;
  Bytes.unsafe_to_string res

let random_name len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    let v = Random.int (26 + 26 + 10) in
    let c =
      if v < 10
      then Char.chr (Char.code '0' + v)
      else if v < 10 + 26
      then Char.chr (Char.code 'a' + (v - 10))
      else Char.chr (Char.code 'A' + (v - 26 - 10)) in
    Bytes.set res i c
  done ;
  Bytes.unsafe_to_string res

let random_perm () =
  match Random.int 6 with
  | 0 -> `Commit
  | 1 -> `Dir
  | 2 -> `Everybody
  | 3 -> `Exec
  | 4 -> `Link
  | 5 -> `Normal
  | _ -> assert false

let random_sha1 () = Digestif.SHA1.of_raw_string (random_string 20)

let entry =
  Alcotest.testable
    (Git.Tree.pp_entry ~pp:Fmt.(const string "()"))
    (Git.Tree.equal_entry ~equal:(fun () () -> true))

let tree =
  Alcotest.testable
    (Git.Tree.pp ~pp:Fmt.(const string "()"))
    (Git.Tree.equal ~equal:(fun () () -> true))

let order =
  Alcotest.test_case "order" `Quick @@ fun () ->
  let lst =
    [
      Git.Tree.entry ~name:"foo.c" `Normal ();
      Git.Tree.entry ~name:"foo" `Dir ();
      Git.Tree.entry ~name:"foo1" `Exec ();
    ] in
  let r0 = Git.Tree.of_list lst in
  let r1 = Git.Tree.to_list r0 in
  Alcotest.(check (list entry)) "list" r1 lst ;
  let r2 = Git.Tree.of_list [] in
  let r2 = Git.Tree.add (List.nth lst 0) r2 in
  let r2 = Git.Tree.add (List.nth lst 1) r2 in
  let r2 = Git.Tree.add (List.nth lst 2) r2 in
  Alcotest.(check tree) "add" r2 r0 ;
  let r2 = Git.Tree.of_list [] in
  let r2 = Git.Tree.add (List.nth lst 2) r2 in
  let r2 = Git.Tree.add (List.nth lst 0) r2 in
  let r2 = Git.Tree.add (List.nth lst 1) r2 in
  let r2 = Git.Tree.add (List.nth lst 1) r2 in
  Alcotest.(check tree) "add" r2 r0 ;
  let empty = Git.Tree.of_list [] in
  let r3 = List.fold_right Git.Tree.add lst empty in
  let r4 = List.fold_right Git.Tree.add (Git.Tree.to_list r0) empty in
  Alcotest.(check tree) "fold" r3 r0 ;
  Alcotest.(check tree) "fold" r4 r0

module Tree = Git.Tree.Make (Git.Hash.Make (Digestif.SHA1))

let tree = Alcotest.testable Tree.pp Tree.equal

(* XXX(dinosaure): this test wants to check if a small internal buffer ([0x200]) is
   enough to parse a large tree object. According the way to construct our tree, the
   biggest alteration is about name (where we can generate a name with [0x100] bytes - so
   an internal buffer of [0x100] bytes is not enough).

   We want to check the generated [angstrom]'s parser by [encore] and see if it resolves
   the alteration as soon as possible. For the Git tree object, the alteration is _committed_
   when we fully parse one entry. *)

let parse_string p str =
  let buffer = Bigstringaf.create 0x100 in
  let queue = Ke.Rke.Weighted.from buffer in
  let state = Angstrom.Unbuffered.parse p in
  let blit src src_off dst dst_off len =
    Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len in
  let rec go pos = function
    | Angstrom.Unbuffered.Done (committed, v) ->
        Ke.Rke.Weighted.N.shift_exn queue committed ;
        if pos = String.length str && Ke.Rke.Weighted.length queue = 0
        then Ok v
        else
          Rresult.R.error_msgf "%d byte(s) remaining"
            (Ke.Rke.Weighted.length queue + (String.length str - pos))
    | Fail (_, _, err) -> Error (`Msg err)
    | Partial { committed; continue } ->
        Ke.Rke.Weighted.N.shift_exn queue committed ;
        Ke.Rke.Weighted.compress queue ;

        if pos = String.length str
        then
          go pos
            (continue ~off:0
               ~len:(Ke.Rke.Weighted.length queue)
               buffer Complete)
        else
          let len =
            min (String.length str - pos) (Ke.Rke.Weighted.available queue)
          in
          let _ =
            Ke.Rke.Weighted.N.push_exn queue ~blit ~length:String.length
              ~off:pos ~len str in
          go (pos + len)
            (continue ~off:0
               ~len:(Ke.Rke.Weighted.length queue)
               buffer Incomplete) in
  go 0 state

let large =
  Alcotest.test_case "large" `Quick @@ fun () ->
  let entries =
    Array.init 2500 (fun _ ->
        let len = 1 + Random.int 255 in
        let name = random_name len in
        let perm = random_perm () in
        let sha1 = random_sha1 () in
        Git.Tree.entry ~name perm sha1) in
  let v0 = Git.Tree.v (Array.to_list entries) in
  let p = Encore.to_angstrom Tree.format in
  let d = Encore.to_lavoisier Tree.format in
  let payload = Encore.Lavoisier.emit_string v0 d in
  match parse_string p payload with
  | Ok v1 -> Alcotest.(check tree) "large tree" v0 v1
  | Error _ -> Alcotest.fail "Error to parse large tree"

let () = Alcotest.run "git-tree" [ ("tree", [ order; large ]) ]
