let () = Random.self_init ()
let pp ppf style h = Fmt.pf ppf "%a " Fmt.(styled style string) h

let pad n x =
  if String.length x > n then x else x ^ String.make (n - String.length x) ' '

let reporter () =
  let report src level ~over k msgf =
    let k _ = over () ; k () in
    let ppf = match level with Logs.App -> Fmt.stdout | _ -> Fmt.stderr in
    let with_stamp h _tags k fmt =
      let dt = Mtime.Span.to_us (Mtime_clock.elapsed ()) in
      Fmt.kpf k ppf
        ("\r%+04.0fus %a %a @[" ^^ fmt ^^ "@]@.")
        dt
        Fmt.(styled `Magenta string)
        (pad 10 @@ Logs.Src.name src)
        Logs_fmt.pp_header (level, h)
    in
    msgf @@ fun ?header ?tags fmt -> with_stamp header tags k fmt
  in
  {Logs.report}

let setup_log level =
  Fmt_tty.setup_std_outputs () ;
  Logs.set_level level ;
  Logs.set_reporter (reporter ()) ;
  ()

let verbose () = setup_log (Some Logs.Debug)
let () = verbose ()

type flow =
  { rd : int -> string option Lwt.t
  ; wr : string -> unit Lwt.t }

let servers : (Uri.t, flow) Hashtbl.t = Hashtbl.create 16

let to_flow (inputs, outputs) =
  let off = ref 0 in
  let buffer = Buffer.create 16 in
  let us = ref inputs in
  let vs = ref outputs in
  let wr str = match !vs with
    | [] -> Alcotest.failf "Unexpected data sended: %S" str
    | expected :: rest ->
      let expected_len = String.length expected in
      let len = min (expected_len - Buffer.length buffer) (String.length str) in
      Buffer.add_substring buffer str 0 len ;

      if Buffer.length buffer < expected_len
      then Lwt.return ()
      else
        ( let has = Buffer.contents buffer in
          Alcotest.(check string) (Fmt.strf "output:%S" expected) expected has
        ; Buffer.clear buffer
        ; vs := rest
        ; Buffer.add_substring buffer str len (String.length str - len)
        ; Lwt.return () ) in
  let rd len = match !us with
    | [] -> Lwt.return None
    | payload :: rest ->
      if String.length payload - !off <= len
      then ( let res = String.sub payload !off (String.length payload - !off) in
             us := rest
           ; off := 0
           ; Lwt.return_some res )
      else
        ( let res = String.sub payload !off len in
          off := !off + len
        ; Lwt.return_some res ) in
  { wr; rd }

module Net : Git.Tcp.NET with type endpoint = Uri.t = struct
  type endpoint = Uri.t
  type socket = flow
  type error = |

  let pp_error : error Fmt.t = fun _ -> function _ -> .

  let read socket buf off len =
    let open Lwt.Infix in
    socket.rd len >>= function
    | Some str ->
      let len = String.length str in
      Bytes.blit_string str 0 buf off len ; Lwt.return_ok len
    | None -> Lwt.return_ok 0 (* XXX(dinosaure): or error? *)

  let write socket buf off len =
    let open Lwt.Infix in
    let str = Bytes.sub_string buf off len in
    socket.wr str >>= fun () -> Lwt.return_ok len

  let socket endpoint =
    let res = Hashtbl.find servers endpoint in
    Lwt.return res

  let close _ = Lwt.return_unit
end

module Endpoint : Git.Sync.ENDPOINT with type t = Net.endpoint = struct
  type t = Net.endpoint

  let uri x : Uri.t = x
end

module type S = sig
  include Git.S

  val u : unit -> t Lwt.t
end

module Make (S : S) = struct
  module Sync = Git.Tcp.Make (Net) (Endpoint) (S)

  let test_clone_non_existing_repository store =
    let payloads =
      [ "0043git-upload-pack /a-non-existing-repository\000host=127.0.0.1:9418\000" ],
      [ "004cERR access denied or repository not exported: /a-non-existing-repository" ] in
    let remote = Uri.of_string "git://127.0.0.1/a-non-existing-repository" in
    Hashtbl.add servers remote (to_flow payloads) ;
    let open Lwt.Infix in
    Sync.clone store ~reference:S.Reference.(master, master) remote >>= function
    | Ok () -> Lwt.return_error (Rresult.R.msg "Unexpected good response from a non-existing repository")
    | Error _ -> Lwt.return_ok ()

  let test_clone_with_one_commit store =
    let payloads =
      [ "0040git-upload-pack /test_remote_repository\000host=127.0.0.1:9418\000"
      ; "008cwant 7cc17765d9ca02032c6452ba126e94a10e0589de multi_ack_detailed thin-pack side-band-64k ofs-delta agent=git/2.0.0 report-status no-done"
      ; "00000000"
      ; "0008done" ],
      [ "00c97cc17765d9ca02032c6452ba126e94a10e0589de HEAD\000\
         multi_ack thin-pack side-band side-band-64k ofs-delta shallow no-progress include-tag multi_ack_detailed symref=HEAD:refs/heads/master agent=git/2.7.4\n"
      ; "003f7cc17765d9ca02032c6452ba126e94a10e0589de refs/heads/master\n"
      ; "0000"
      ; "0008NAK\n"
      ; "0008NAK\n"
      (* PACK file, as receiver, it just needs to be valid. *)
      ; "\x30\x30\x34\x61\x02\x44\xc3\xa9\x63\x6f\x6d\x70\x74\x65\x20\x64"    (* 004a.D..compte d *)
      ; "\x65\x73\x20\x6f\x62\x6a\x65\x74\x73\x3a\x20\x33\x2c\x20\x66\x61"    (* es objets: 3, fa *)
      ; "\x69\x74\x2e\x0a\x54\x6f\x74\x61\x6c\x20\x33\x20\x28\x64\x65\x6c"    (* it..Total 3 (del *)
      ; "\x74\x61\x20\x30\x29\x2c\x20\x72\x65\x75\x73\x65\x64\x20\x30\x20"    (* ta 0), reused 0  *)
      ; "\x28\x64\x65\x6c\x74\x61\x20\x30\x29\x0a\x30\x30\x64\x64\x01\x50"    (* (delta 0).00dd.P *)
      ; "\x41\x43\x4b\x00\x00\x00\x02\x00\x00\x00\x03\x97\x0d\x78\x9c\xad"    (* ACK..........x.. *)
      ; "\x8c\x4b\x0a\x02\x31\x10\x05\xf7\x39\x45\xef\x85\xa1\xf3\x4f\x40"    (* .K..1...9E....O@ *)
      ; "\x44\xf0\x06\xde\xa0\xd3\x06\x27\x30\x99\x40\xcc\xdc\xdf\xa0\x1b"    (* D......'0.@..... *)
      ; "\x0f\xe0\xea\xf1\xaa\xa0\x46\xcf\x19\xbc\x4b\x52\xc9\xf8\x48\x29"    (* ......F...KR..H) *)
      ; "\x78\x8b\xd9\x69\x29\xe7\x67\x43\x1a\x1d\x46\x63\xb5\xe7\x10\x23"    (* x..i).gC..Fc...# *)
      ; "\xa3\xa0\x63\xac\xad\xc3\x8d\x36\x7a\x71\x49\x79\x0c\x82\x7b\xab"    (* ..c....6zqIy..{. *)
      ; "\x54\x76\x38\xf7\xcf\x2e\xfc\xe3\xae\xcf\x89\xb6\x85\x5b\xbd\x80"    (* Tv8..........[.. *)
      ; "\xb4\xd6\x28\x1f\x4c\x8c\x70\x42\x85\x28\x26\xad\x65\x8c\xfc\xa7"    (* ..(.L.pB.(&.e... *)
      ; "\x9c\x58\x8f\x4a\x3b\x7c\xab\xe2\x0d\x57\xbe\x44\x11\xa3\x02\x78"    (* .X.J;|...W.D...x *)
      ; "\x9c\x33\x34\x30\x30\x33\x31\x51\x48\x2d\x28\xce\xcc\xc9\xcf\x63"    (* .340031QH-(....c *)
      ; "\x78\x36\xf7\xd1\xec\x4d\x17\xaf\x39\x7b\x77\x6b\xae\x2b\x8f\xba"    (* x6...M..9{wk.+.. *)
      ; "\x71\xe8\x49\x4f\xf0\x44\x00\xf9\x72\x10\x52\x30\x78\x9c\x03\x00"    (* q.IO.D..r.R0x... *)
      ; "\x00\x00\x00\x01\xa8\x5a\x5b\xd4\xab\xb8\x9f\xc2\xe5\xe1\x9a\x03"    (* .....Z[......... *)
      ; "\xdc\x80\x2d\x52\x2e\x88\x2c\x30\x30\x30\x36\x01\xd1\x30\x30\x30"    (* ..-R..,0006..000 *)
      ; "\x30" (* 0 *) ] in
    let a, b = payloads in
    let payloads = b, a in
    let remote = Uri.of_string "git://127.0.0.1/test_remote_repository" in
    Hashtbl.add servers remote (to_flow payloads) ;
    let open Lwt.Infix in
    Sync.clone store ~reference:S.Reference.(master, master) remote >>= function
    | Ok () -> Lwt.return_ok ()
    | Error _ -> Lwt.return_error (Rresult.R.msg "Unexpected bad response from test_remote_repository")

  (* XXX(dinosaure): a fragile test which works because generation of PACK is determinist. *)
  let test_push_non_bare_repository store =
    let payloads =
      [ "0041git-receive-pack /test_remote_repository\000host=127.0.0.1:9418\000"
      ; "009d7cc17765d9ca02032c6452ba126e94a10e0589de c18b835ec91c732706786d24a5d4c1a0c36b3b37 refs/heads/master\000side-band-64k ofs-delta agent=git/2.0.0 report-status"
      ; "\x30\x30\x30\x30\x50\x41\x43\x4b\x00\x00\x00\x02\x00\x00\x00\x03"   (* 0000PACK........ *)
      ; "\x33\x78\x9c\x04\xc1\xb1\x0d\x00\x00\x00\x40\xb0\x5d\xe2\x60\xdf"   (* 3x........@.].`. *)
      ; "\x6b\x93\x03\x00\x00\xff\xff\x01\x92\x00\x91\xa0\x04\x78\x9c\x2c"   (* k............x., *)
      ; "\xca\x21\x0e\x40\x60\x14\x00\xe0\x97\x04\x9b\x1b\x08\x22\xed\xd9"   (* .!.@`........... *)
      ; "\xde\x38\x80\x28\x0a\x4c\x17\x6c\x86\x4d\x60\xd3\x54\x8a\x6c\x6c"   (* .8.(.L.l.M`.T.ll *)
      ; "\x2e\x20\x68\xa6\xc9\x8a\x62\x7f\x36\xb3\x39\x82\x44\xf1\xe5\x4f"   (* . h...b.6.9.D..O *)
      ; "\x45\xd4\x88\x24\x2f\x4e\xfc\x20\x0a\xe1\x6a\x8f\x66\xdc\x76\xc3"   (* E..$/N. ..j.f.v. *)
      ; "\x2c\x95\x21\x75\xd9\x72\x56\x56\xfd\x97\x0c\x6c\x87\x09\x7d\xa1"   (* ,.!u.rVV...l..}. *)
      ; "\xaf\x5d\x0e\x93\x7c\x73\x33\x2f\xbe\xcf\x17\x00\x00\xff\xff\x5d"   (* .]..|s3/.......] *)
      ; "\xe2\x1b\x95\x9c\x0d\x78\x9c\x9c\x8c\x41\x6a\x04\x21\x10\x45\xf7"   (* .....x...Aj.!.E. *)
      ; "\x9e\xc2\x7d\x20\x94\x15\xb5\x14\x42\x98\xab\x94\xd5\x35\xdd\xc2"   (* ..} ....B....5.. *)
      ; "\xa8\x8d\x71\xee\x1f\xcf\x90\xb7\xf8\xf0\x17\xef\xad\xa9\x6a\x03"   (* ..q...........j. *)
      ; "\x24\x50\x2e\xfe\x99\x90\xe8\x70\x90\x7c\x09\xfe\x49\x2e\xca\x7e"   (* $P.....p.|..I..~ *)
      ; "\x19\x25\x0b\x10\x16\x40\x73\xf3\xd4\xbe\x2c\x89\x38\xa2\x18\x8e"   (* .%...@s...,.8... *)
      ; "\x2c\x0c\x08\x5f\x28\xd1\x07\x2c\xec\x30\x6a\xf6\xec\x40\x21\xa4"   (* ,.._(..,.0j..@!. *)
      ; "\x7c\xa8\xe1\xf7\xba\xc6\xb4\xf7\xfb\xf7\xda\xf3\x7a\xd9\xef\x3a"   (* |...........z..: *)
      ; "\x5b\xed\x8f\x71\x6b\x6f\x75\xf2\xa9\x9f\x63\x9e\x3f\xd6\x85\xe0"   (* [..qkou...c.?... *)
      ; "\x13\xee\x06\xd9\x0f\xd8\x18\x19\xad\xd5\xb5\xf4\x1f\xae\x31\x6c"   (* ..............1l *)
      ; "\xe5\xe2\x7e\xea\x5f\x00\x00\x00\xff\xff\xf8\xc1\x41\xf1\x47\x7d"   (* ..~._.......A.G} *)
      ; "\x6d\xc6\xde\x3b\x1b\xe8\x53\x7d\x37\x27\xd2\x5e\xa9\x3c\x04\x48"   (* m..;..S}7'.^.<.H *)
      ; "\xc7\x7d" ],
      [ "008e7cc17765d9ca02032c6452ba126e94a10e0589de refs/heads/master\000report-status delete-refs side-band-64k quiet atomic ofs-delta agent=git/2.7.4\n"
      ; "0000"
      ; "001e\002error non-bare repository"
      ; "0050\001000eunpack ok\n0039ng refs/heads/master branch is currently checked out\n00000000" ] in
    let a, b = payloads in
    let payloads = b, a in
    let remote = Uri.of_string "git://127.0.0.1/test_remote_repository" in
    Hashtbl.add servers remote (to_flow payloads) ;
    let author = { Git.User.name= "push pull"
                 ; email= "irmin@openmirage.org"
                 ; date= 1554824527L, None } in
    let parent = S.Hash.of_hex "7cc17765d9ca02032c6452ba126e94a10e0589de" in
    let res =
      let open Lwt_result.Infix in
      S.write store S.Value.(blob (Blob.of_string "y\r\n")) >>= fun (hash_blob, _) ->
      Fmt.epr "Hash of blob %a.\n%!" S.Hash.pp hash_blob ;
      S.write store S.Value.(tree (Tree.of_list [ S.Value.Tree.entry "epsilon" `Normal (S.Hash.of_hex "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391")
                                                ; S.Value.Tree.entry "x" `Normal hash_blob ])) >>= fun (hash_tree, _) ->
      Fmt.epr "Hash of tree %a.\n%!" S.Hash.pp hash_tree ;
      S.write store S.Value.(commit (Commit.make ~tree:hash_tree ~author ~committer:author ~parents:[ parent ] "\na change")) in
    let open Lwt.Infix in
    res >>= function
    | Error _ -> assert false
    | Ok (hash_commit, _) ->
      let push _ = Lwt.return ([], [ `Update (parent, hash_commit, S.Reference.master) ]) in
      Sync.push store ~push remote >>= function
      | Ok _ -> Lwt.return_ok ()
      | Error _ -> Lwt.return_error (Rresult.R.msg "Unexpected bad response from test_remote_repository")

  let test_push_access_denied store =
    let payloads =
      [ "003cgit-receive-pack /mirage/ocaml-git\000host=github.com:9418\000" ],
      [ "\x00\x00\x00\x00\x00\x00"
      ; "0070ERR \n  You can't push to git://github.com/mirage/ocaml-git.git\n\
         Use https://github.com/mirage/ocaml-git.git"
      ; "\x00\x00\x00\x00\x00\x00"
      ; "\x00\x00\x00\x00\x00\x00" ] in
    let a, b = payloads in
    let payloads = b, a in
    let remote = Uri.of_string "git://github.com/mirage/ocaml-git" in
    Hashtbl.add servers remote (to_flow payloads) ;
    let null = S.Hash.of_raw_string (String.make S.Hash.digest_size '\x00') in
    let push _ = Lwt.return ([], [ `Update (null, null, S.Reference.master) ]) in
    let open Lwt.Infix in
    Sync.push store ~push remote >>= function
    | Ok _ -> Lwt.return_error (Rresult.R.msg "Unexpected good response from test test_push_access_denied")
    | Error _ -> Lwt.return_ok ()



  let tests () =
    let test ~name test =
      Alcotest.test_case name `Quick @@ fun () ->
      match Lwt_main.run Lwt.Infix.(S.u () >>= test) with
      | Ok () -> ()
      | Error (`Msg err) -> Alcotest.fail err in
    "smart-regression",
    [ test ~name:"non-existing-repository" test_clone_non_existing_repository
    ; test ~name:"clone-repository" test_clone_with_one_commit
    ; test ~name:"push-non-bare-repository" test_push_non_bare_repository
    ; test ~name:"push-access-denied" test_push_access_denied ]
end
