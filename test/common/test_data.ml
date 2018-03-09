module type SOURCE = sig

  val pack: Fpath.t
  val refs: Fpath.t
  val idx: Fpath.t
  val name: string
end

module type S = sig

  include Git.S
  val create: Fpath.t -> (t, error) result Lwt.t
end

let pp_process_status ppf = function
  | Unix.WEXITED i   -> Fmt.pf ppf "exit %d" i
  | Unix.WSIGNALED i -> Fmt.pf ppf "signal %d" i
  | Unix.WSTOPPED i  -> Fmt.pf ppf "stop %d" i

let drain buf chan =
  try while true do Buffer.add_channel buf chan 1 done
  with End_of_file -> ()

let output_of_command ?(env = Unix.environment ()) ?(input = (fun _ -> ())) command =
  let ic, oc, ec = Unix.open_process_full command env in
  let () = input oc in
  close_out oc;
  let buf = Buffer.create 512 in
  drain buf ic;
  drain buf ec;
  let s = Unix.close_process_full (ic, oc, ec) in
  let r = Buffer.contents buf in
  Logs.debug (fun l -> l "[run] %s (%a):@ @[%s@]" command pp_process_status s r);
  r

module Make0 (Source: SOURCE) (Store: S) = struct

  open Lwt.Infix
  let ( >?= ) = Lwt_result.bind
  let ( >!= ) = Lwt_result.bind_lwt_err

  let stream_of_file path =
    let size = 0x8000 in
    let btmp = Bytes.create size in
    let ic = open_in_bin (Fpath.to_string path) in

    (fun () ->
       let len = input ic btmp 0 size in
       if len = 0
       then begin
         close_in ic;
         Lwt.return None
       end else begin
         let ctmp = Cstruct.create len in
         Cstruct.blit_from_bytes btmp 0 ctmp 0 len;
         Lwt.return (Some ctmp)
       end)

  let store_err err = Lwt.return (`Store err)

  let pp_error ppf = function
    | `Store err -> Store.pp_error ppf err

  let hashes_of_pack idx =
    let command = Fmt.strf "git verify-pack -v %a" Fpath.pp idx in
    let output = output_of_command command in
    let lines = Astring.String.cuts ~sep:"\n" output in
    let is_hash s =
      Astring.String.for_all
        (function
          | 'a' .. 'f' | 'A' .. 'F' | '0' .. '9' -> true
          | _ -> false) s
      && String.length s = (Store.Hash.Digest.length * 2) in

    List.fold_left
      (fun acc line ->
         match Astring.String.cut ~sep:" " line with
         | Some (hash, _) when is_hash hash -> (Store.Hash.of_hex hash) :: acc
         | _ -> acc)
      [] lines

  let load_file path =
    let ic = open_in_bin (Fpath.to_string path) in
    let len = in_channel_length ic in
    let res = Bytes.create len in
    really_input ic res 0 len;
    close_in ic;
    Bytes.unsafe_to_string res

  module Common = Test_common.Make(Store)

  let import root () =
    Store.create root >!= store_err >?= fun t ->
    let stream = stream_of_file Source.pack in
    Store.Pack.from t stream >!= store_err >?= fun _ ->
    Store.list t >>= fun hashes ->
    let hashes' = hashes_of_pack Source.idx in
    let () = Common.assert_keys_equal "import" hashes hashes' in
    Lwt.return (Ok t)

  let verify_index_file () =
    let module D = Git.Index_pack.Decoder(Store.Hash) in
    let module R = Git.Radix.Make(struct
        type t = Store.Hash.t

        let get = Store.Hash.get
        let length _ = Store.Hash.Digest.length
      end) in
    let ic = open_in_bin (Fpath.to_string Source.idx) in
    let size = 0x8000 in
    let dtmp = Bytes.create size in
    let ctmp = Cstruct.create size in

    let rec go acc state = match D.eval ctmp state with
      | `Await state ->
        let len = input ic dtmp 0 size in
        Cstruct.blit_from_bytes dtmp 0 ctmp 0 len;
        go acc (D.refill 0 len state)
      | `Hash (state, (hash, crc, off)) ->
        let acc = R.bind acc hash (crc, off) in
        go acc state
      | `Error (_, err) ->
        Alcotest.failf "Got an error when we parse the IDX file %a: %a."
          Fpath.pp Source.idx
          D.pp_error err
      | `End (_, hash) -> (acc, hash)
    in

    let (tree, hash_pack) = go R.empty (D.make ()) in
    let () = close_in ic in

    let module E = Git.Index_pack.Encoder(Store.Hash) in
    let buf = Git.Buffer.create 1024 in

    let rec go ctx state = match E.eval ctmp state with
      | `Flush t ->
        Git.Buffer.add buf (Cstruct.sub ctmp 0 (E.used_out t));
        Store.Hash.Digest.feed ctx (Cstruct.sub ctmp 0 (E.used_out t));
        go ctx (E.flush 0 (Cstruct.len ctmp) t)
      | `Error (_, err) ->
        Alcotest.failf "Got an error when we encode the IDX file %a: %a."
          Fpath.pp Source.idx
          E.pp_error err
      | `End t ->
        if E.used_out t > 0
        then begin
          Git.Buffer.add buf (Cstruct.sub ctmp 0 (E.used_out t));
          Store.Hash.Digest.feed ctx (Cstruct.sub ctmp 0 (E.used_out t));
        end
    in

    let () = go (Store.Hash.Digest.init ()) (E.default (R.to_sequence tree) hash_pack) in

    if String.equal (load_file Source.idx) (Git.Buffer.contents buf)
    then Lwt.return (Ok ())
    else Alcotest.failf "Produced IDX file is not the same from the IDX file: %a."
           Fpath.pp Source.idx

  let run f =
    Lwt_main.run
      (f () >>= function
        | Ok v -> Lwt.return v
        | Error err -> Alcotest.failf "Got an error: %a." pp_error err)

  let root = Fpath.(v "test-data" / Source.name)

  let verify_unpack () = import root () >?= fun _ -> Lwt.return (Ok ())

  let test_index_file () = run verify_index_file
  let test_unpack () = run verify_unpack
end

module Usual = struct
  let pack = Fpath.(v ".." / "data" / "pack.pack")
  let idx = Fpath.(v ".." / "data" / "pack.idx")
  let refs = Fpath.(v ".." / "data" / "pack.refs")

  let name = "usual"
end

module UsualRevlist (S: Git.S) = struct
  type hash = S.Hash.t and t = [ `Diff of hash * hash | `New of hash ]

  let lst =
    [ "bottom to top",
      `Diff (S.Hash.of_hex "381efb0df07110ad3a1b81a33d698c98d9fa18f3",
             S.Hash.of_hex "3ae8d4d9cdf28d0cd6b453da991fb661ce05de08")
    ; "top to bottom",
      `Diff (S.Hash.of_hex "3ae8d4d9cdf28d0cd6b453da991fb661ce05de08",
             S.Hash.of_hex "381efb0df07110ad3a1b81a33d698c98d9fa18f3")
    ; "middle diff 00",
      `Diff (S.Hash.of_hex "4b01e27cd1fd4b9390c9a8dccb8fa415c72770f5",
             S.Hash.of_hex "4ee53344e6faafd2e5e3b86ec762f180db4a2c3b")
    ; "middle diff 01",
      `Diff (S.Hash.of_hex "726e053b15133c5721795ecacf6be9ad616d1ec5",
             S.Hash.of_hex "fe88134bf478438e4eb6e2c62c2743eab78ee4e2") ]
end

(* XXX(dinosaure): I reaaly would like to test bomb, however, git segfault with
   this repository and we need git to compare results. *)

module BombRevlist (S: Git.S) = struct

  type hash = S.Hash.t and t = [ `Diff of hash * hash | `New of hash ]

  let lst =
    [ "bottom to top",
      `Diff (S.Hash.of_hex "45546f17e5801791d4bc5968b91253a2f4b0db72",
             S.Hash.of_hex "7af99c9e7d4768fa681f4fe4ff61259794cf719b")
    ; "top to bottom",
      `Diff (S.Hash.of_hex "7af99c9e7d4768fa681f4fe4ff61259794cf719b",
             S.Hash.of_hex "45546f17e5801791d4bc5968b91253a2f4b0db72")
    ; "middle diff 00",
      `Diff (S.Hash.of_hex "18ed56cbc5012117e24a603e7c072cf65d36d469",
             S.Hash.of_hex "7af99c9e7d4768fa681f4fe4ff61259794cf719b")
    ; "middle diff 01",
      `Diff (S.Hash.of_hex "45546f17e5801791d4bc5968b91253a2f4b0db72",
             S.Hash.of_hex "18ed56cbc5012117e24a603e7c072cf65d36d469") ]
end

module Bomb = struct
  let pack = Fpath.(v ".." / "data" / "bomb.pack")
  let idx = Fpath.(v ".." / "data" / "bomb.idx")
  let refs = Fpath.(v ".." / "data" / "bomb.refs")

  let name = "bomb"
end

let suite name (module F: SOURCE) (module S: S) =
  let module T = Make0(F)(S) in
  (Fmt.strf "%s: %s" name F.name),
  [ "index-pack", `Quick, T.test_index_file
  ; "unpack", `Quick, T.test_unpack ]
