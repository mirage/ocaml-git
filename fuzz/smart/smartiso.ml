open Crowbar
module S = Git_unix.FS

module Make (Hash: Git.HASH) (Reference: Git.Reference.S) =
struct
  module Common = Git.Smart.Common(Hash)(Reference)
  module Encoder = Git.Smart.Encoder(Hash)(Reference)(Common)
  module Decoder = Git.Smart.Decoder(Hash)(Reference)(Common)
end

module Smart = Make(S.Hash)(S.Reference)

let hash =
  map [bytes] (fun hash ->
      if String.length hash = S.Hash.Digest.length
      then S.Hash.of_string hash
      else bad_test ())

exception Have

let satisfy f s =
  let l = String.length s in

  try
    for i = 0 to l - 1
    do if f (String.get s i) then raise Have done; false
  with Have -> true

let reference =
  map [bytes] (fun str ->
      if satisfy
           (function ' ' | '~' | '^' | ':' | '?' | '*' -> true
                     | chr -> let code = Char.code chr in
                              if code < 32 || code > 126
                              then true
                              else false)
           str
      then bad_test ();

      try let path = Fpath.v str in
          match Fpath.segs path with
          | _ :: _ -> S.Reference.of_path path
          | [] -> bad_test ()
      with Invalid_argument _ -> bad_test ())

let capability =
  map [range 21] (function
      | 0  -> `Multi_ack
      | 1  -> `Multi_ack_detailed
      | 2  -> `No_done
      | 3  -> `Thin_pack
      | 4  -> `Side_band
      | 5  -> `Side_band_64k
      | 6  -> `Ofs_delta
      | 7  -> `Agent "git/2.0.0"
      | 8  -> `Shallow
      | 9  -> `Deepen_since
      | 10 -> `Deepen_not
      | 11 -> `No_progress
      | 12 -> `Include_tag
      | 13 -> `Report_status
      | 14 -> `Delete_refs
      | 15 -> `Quiet
      | 16 -> `Atomic
      | 17 -> `Push_options
      | 18 -> `Allow_tip_sha1_in_want
      | 19 -> `Allow_reachable_sha1_in_want
      | 20 -> `Push_cert "<nonce>"
      | _  -> bad_test ())

let capabilities =
  map [list1 capability] (fun capabilities ->
      let have c x = List.exists ((=) x) c in

      if (have capabilities `Side_band && have capabilities `Side_band_64k)
       || (have capabilities `Multi_ack && have capabilities `Multi_ack_detailed)
      then bad_test ()
      else capabilities)

let advertised_refs =
  let r = map [hash; reference; bool] (fun hash reference bool -> (hash, reference, bool)) in
  map [list hash; list r; capabilities]
    (fun shallow references capabilities ->
       { Smart.Common.shallow
       ; refs = references
       ; capabilities })

let shallow_update =
  map [list hash; list hash] (fun shallow unshallow -> { Smart.Common.shallow; unshallow; })

let ack ack_mode =
  match ack_mode with
  | `Ack -> map [hash] (fun hash -> hash, `ACK)
  | `Multi_ack -> map [hash] (fun hash -> hash, `Continue)
  | `Multi_ack_detailed -> map [hash; range 2] (fun hash -> function
                               | 0 -> hash, `Ready
                               | 1 -> hash, `Common
                               | _ -> bad_test ())

let acks ack_mode =
  map [ list hash
      ; list hash
      ; (match ack_mode with
         | `Ack -> map [ ack ack_mode ] (fun x -> [ x ])
         | `Multi_ack
         | `Multi_ack_detailed -> list1 (ack ack_mode)) ]
    (fun shallow unshallow acks -> { Smart.Common.shallow; unshallow; acks; })

let negociation_result =
  map [range 3; hash; bytes] (fun k hash err ->
      match k with
      | 0 -> Smart.Common.NAK
      | 1 -> Smart.Common.ACK hash
      | 2 ->
         if String.length err = 0 || satisfy (function '\000' .. '\032' | '\127' -> true | _ -> false) err
         then bad_test ();

         Smart.Common.ERR err
      | _ -> bad_test ())

let report_status =
  let error_msg = map [bytes] (fun s -> try let _ = String.index s '\n' in bad_test () with Not_found -> if String.length s = 0 then bad_test (); s) in
  let result a b = choose [map [a] (fun a -> Ok a); map [b] (fun b -> Error b)] in
  let pair a b = map [a; b] (fun a b -> (a, b)) in
  map [ option error_msg; list1 (result reference (pair reference error_msg))]
    (fun unpack commands ->
      let unpack = match unpack with
        | Some err -> Error err
        | None -> Ok () in
      { Smart.Common.unpack; commands; })

let deepen =
  map [option (range 3); int; int64; reference]
    (fun i n t r -> match i with
                    | Some 0 when n > 0 -> Some (`Depth n)
                    | Some 1 when Int64.compare t 0L > 0 -> Some (`Timestamp t)
                    | Some 2 -> Some (`Ref r)
                    | Some _ -> bad_test ()
                    | None -> None)

let upload_request =
  map [list1 hash; capabilities; list hash; deepen]
    (fun wants capabilities shallow deep ->
      let want, wants = List.hd wants, List.tl wants in
      { Smart.Common.want = want, wants
      ; capabilities
      ; shallow
      ; deep })

let () =
  add_test
    ~name:"upload-request"
    [ upload_request ]
  @@ fun upload_request ->
     let s = Smart.Encoder.to_string (`UploadRequest upload_request) in

     match Smart.Decoder.of_string s Smart.Decoder.Upload_request with
     | Ok v ->
        check_eq ~pp:Smart.Common.pp_upload_request ~eq:Smart.Common.equal_upload_request v upload_request
     | Error (err, _, committed) ->
        Fmt.(pf stderr) "Encoded to: %a.\n%!"
          (Fmt.hvbox @@ Git.Minienc.pp_scalar ~get:String.get ~length:String.length) s;
        fail (Fmt.strf "%a (committed:%d)" Smart.Decoder.pp_error err committed)

let () =
  add_test
    ~name:"advertised_refs"
    [ advertised_refs ]
  @@ fun advertised_refs ->
     let s = Smart.Encoder.to_string (`Advertised_refs advertised_refs) in

     match Smart.Decoder.of_string s Smart.Decoder.ReferenceDiscovery with
     | Ok v ->
        check_eq ~pp:Smart.Common.pp_advertised_refs ~eq:Smart.Common.equal_advertised_refs v advertised_refs
     | Error (err, _, committed) ->
        fail (Fmt.strf "%a with (committed:%d) %a" Smart.Decoder.pp_error err committed
                (Fmt.hvbox (Git.Minienc.pp_scalar ~get:String.get ~length:String.length)) s)

let () =
  add_test
    ~name:"shallow_update"
    [ shallow_update ]
  @@ fun shallow_update ->
     let s = Smart.Encoder.to_string (`Shallow_update shallow_update) in

     match Smart.Decoder.of_string s Smart.Decoder.ShallowUpdate with
     | Ok v ->
        check_eq ~pp:Smart.Common.pp_shallow_update ~eq:Smart.Common.equal_shallow_update v shallow_update
     | Error (err, _, committed) ->
        fail (Fmt.strf "%a with (committed:%d) %a" Smart.Decoder.pp_error err committed
                (Fmt.hvbox (Git.Minienc.pp_scalar ~get:String.get ~length:String.length)) s)

let () =
  add_test
    ~name:"negociation_result"
    [ negociation_result ]
  @@ fun negociation_result ->
     let s = Smart.Encoder.to_string (`Negociation_result negociation_result) in

     match Smart.Decoder.of_string s Smart.Decoder.NegociationResult with
     | Ok v ->
        check_eq ~pp:Smart.Common.pp_negociation_result ~eq:Smart.Common.equal_negociation_result v negociation_result
     | Error (err, _, committed) ->
        fail (Fmt.strf "%a with (committed:%d) %a" Smart.Decoder.pp_error err committed
                (Fmt.hvbox (Git.Minienc.pp_scalar ~get:String.get ~length:String.length)) s)

type ack_mode = [ `Ack | `Multi_ack | `Multi_ack_detailed ]

let acks (ack_mode:ack_mode) =
  let ack_mode_to_string = function
    | `Ack -> "ack"
    | `Multi_ack -> "multi-ack"
    | `Multi_ack_detailed -> "multi-ack-detailed" in
  add_test
    ~name:(Fmt.strf "acks (mode:%s)" (ack_mode_to_string ack_mode))
    [ acks ack_mode  ]
  @@ fun acks ->
     let s = Smart.Encoder.to_string (`Negociation acks) in
     let h = S.Hash.Set.of_list (List.map fst acks.Smart.Common.acks) in

     match Smart.Decoder.of_string s (Smart.Decoder.Negociation (h, ack_mode)) with
     | Ok v -> 
        check_eq ~pp:Smart.Common.pp_acks ~eq:Smart.Common.equal_acks v acks
     | Error (err, _, committed) ->
        Fmt.(pf stderr) "Encoded to: %a.\n%!"
          (Fmt.hvbox @@ Git.Minienc.pp_scalar ~get:String.get ~length:String.length) s;
        fail (Fmt.strf "%a (committed:%d)" Smart.Decoder.pp_error err committed)

let () = acks `Ack
let () = acks `Multi_ack
let () = acks `Multi_ack_detailed

let report_status sideband =
  let sideband_to_string = function
    | `No_multiplexe -> "no-multiplexe"
    | `Side_band -> "side-band"
    | `Side_band_64k -> "side-band-64k" in
  add_test
    ~name:(Fmt.strf "report-status (side-band:%s)" (sideband_to_string sideband))
    [ report_status ]
  @@ fun report_status ->
     let s = Smart.Encoder.to_string (`Report_status (sideband, report_status)) in

     match Smart.Decoder.of_string s (Smart.Decoder.ReportStatus sideband) with
     | Ok v ->
        check_eq ~pp:Smart.Common.pp_report_status ~eq:Smart.Common.equal_report_status v report_status
     | Error (err, _, committed) ->
        Fmt.(pf stderr) "Encoded to: %a.\n%!"
          (Fmt.hvbox @@ Git.Minienc.pp_scalar ~get:String.get ~length:String.length) s;
        fail (Fmt.strf "%a (committed:%d)" Smart.Decoder.pp_error err committed)

let () = report_status `No_multiplexe
let () = report_status `Side_band
let () = report_status `Side_band_64k
