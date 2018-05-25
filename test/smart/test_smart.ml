let rec zip la lb = match la, lb with
  | [], [] -> []
  | xa :: ra, xb :: rb ->
    (xa, xb) :: zip ra rb
  | _, _ -> invalid_arg "zip"

module Make (S: Git.S) =
struct

  module Smart =
  struct
    module Common = Git.Smart.Common(S.Hash)(S.Reference)
    module Encoder = Git.Smart.Encoder(S.Hash)(S.Reference)(Common)
    module Decoder = Git.Smart.Decoder(S.Hash)(S.Reference)(Common)
  end

  let generate_hash () =
    let t = Unix.gettimeofday () in
    let cs = Cstruct.create 8 in
    Cstruct.BE.set_uint64 cs 0 Int64.(of_float (t *. 1000.));
    Nocrypto.Rng.reseed cs;
    Nocrypto.Rng.generate S.Hash.Digest.length |> Cstruct.to_string |> S.Hash.of_string

  let generate_hashes n =
    let ar = Array.init n (fun _ -> generate_hash ()) in
    Array.to_list ar

  let () = Random.self_init ()

  let references =
    [ "refs/heads/master"
    ; "refs/remotes/origin/master"
    ; "refs/remotes/upstream/master"
    ; "refs/heads/smart-protocol"
    ; "refs/heads/server"
    ; "refs/heads/smart-tests"
    ; "refs/remotes/origin/smart-protocol"
    ; "refs/remotes/origin/smart-tests"
    ; "refs/remotes/origin/server"
    ; "refs/remotes/upstream/server"
    ; "refs/remotes/upstream/clean" ]
    |> List.map S.Reference.of_string 

  module Data =
  struct
    let capabilities =
      [ `Multi_ack_detailed
      ; `Thin_pack
      ; `Side_band_64k
      ; `Ofs_delta
      ; `Agent "git/2.0.0"
      ; `Report_status
      ; `No_done ]

    let advertised_refs0, advertised_refs0_result =
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; refs = List.map (fun reference -> (generate_hash (), reference, Random.bool ())) references
      ; capabilities }, `Equal

    let advertised_refs1, advertised_refs1_result =
      { Smart.Common.shallow = []
      ; refs = []
      ; capabilities }, `Equal

    let advertised_refs2, advertised_refs2_result =
      { Smart.Common.shallow = []
      ; refs = List.map (fun reference -> (generate_hash (), reference, Random.bool ())) references
      ; capabilities = [] }, `Error

    let advertised_refs3, advertised_refs3_result =
      { Smart.Common.shallow = []
      ; refs = []
      ; capabilities = [] }, `Error

    let advertised_refs_datas =
      [ advertised_refs0, advertised_refs0_result
      ; advertised_refs1, advertised_refs1_result
      ; advertised_refs2, advertised_refs2_result
      ; advertised_refs3, advertised_refs3_result ]

    let shallow_update0, shallow_update0_result =
      { Smart.Common.shallow = []
      ; unshallow = [] }, `Equal

    let shallow_update1, shallow_update1_result =
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = [] }, `Equal

    let shallow_update2, shallow_update2_result =
      { Smart.Common.shallow = []
      ; unshallow = generate_hashes (Random.int 10) }, `Equal

    let shallow_update3, shallow_update3_result =
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = generate_hashes (Random.int 10) }, `Equal

    let shallow_update_datas =
      [ shallow_update0, shallow_update0_result
      ; shallow_update1, shallow_update1_result
      ; shallow_update2, shallow_update2_result ]

    let acks0, acks0_hashes, acks0_kind, acks0_result =
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = generate_hashes (Random.int 10)
      ; acks = [] }, [], `Multi_ack_detailed, `Equal

    let acks1, acks1_hashes, acks1_kind, acks1_result =
      let hashes = generate_hashes 1 in
      let details = [ `Ready ] in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = generate_hashes (Random.int 10)
      ; acks = zip hashes details }, hashes, `Multi_ack_detailed, `Equal

    let acks2, acks2_hashes, acks2_kind, acks2_result =
      let hashes = generate_hashes 2 in
      let details = [ `Common; `Ready ] in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = generate_hashes (Random.int 10)
      ; acks = zip hashes details }, hashes, `Multi_ack_detailed, `Equal

    let acks3, acks3_hashes, acks3_kind, acks3_result =
      let hashes = generate_hashes 5 in
      let details = [ `Common
                    ; `Common
                    ; `Common
                    ; `Common
                    ; `Ready ] in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = generate_hashes (Random.int 10)
      ; acks = zip hashes details }, hashes, `Multi_ack_detailed, `Equal

    let acks4, acks4_hashes, acks4_kind, acks4_result =
      let hashes = generate_hashes 5 in
      let details = [ `Continue
                    ; `Continue
                    ; `Continue
                    ; `Continue
                    ; `Continue ] in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = generate_hashes (Random.int 10)
      ; acks = zip hashes details }, hashes, `Multi_ack, `Equal

    let acks5, acks5_hashes, acks5_kind, acks5_result =
      acks3, acks3_hashes, `Multi_ack, `Error

    let acks6, acks6_hashes, acks6_kind, acks6_result =
      acks4, acks4_hashes, `Multi_ack_detailed, `Error

    let acks7, acks7_hashes, acks7_kind, acks7_result =
      let hashes = [ generate_hash () ] in
      let details = [ `ACK ] in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = generate_hashes (Random.int 10)
      ; acks = zip hashes details }, hashes, `Ack, `Equal

    let acks8, acks8_hashes, acks8_kind, acks8_result =
      let hashes = generate_hashes 2 in
      let details = [ `ACK; `ACK; ] in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; unshallow = generate_hashes (Random.int 10)
      ; acks = zip hashes details }, hashes, `Ack, `Error

    let acks_datas =
      [ acks0, acks0_hashes, acks0_kind, acks0_result
      ; acks1, acks1_hashes, acks1_kind, acks1_result
      ; acks2, acks2_hashes, acks2_kind, acks2_result
      ; acks3, acks3_hashes, acks3_kind, acks3_result
      ; acks4, acks4_hashes, acks4_kind, acks4_result
      ; acks5, acks5_hashes, acks5_kind, acks5_result
      ; acks6, acks6_hashes, acks6_kind, acks6_result
      ; acks7, acks7_hashes, acks7_kind, acks7_result
      ; acks8, acks8_hashes, acks8_kind, acks8_result ]

    let negociation_result0, negociation_result_result0 =
      Smart.Common.NAK, `Equal

    let negociation_result1, negociation_result_result1 =
      Smart.Common.ACK (generate_hash ()), `Equal

    let negociation_result2, negociation_result_result2 =
      Smart.Common.ERR "invalid negotiation", `Equal

    let negociation_result_datas =
      [ negociation_result0, negociation_result_result0
      ; negociation_result1, negociation_result_result1
      ; negociation_result2, negociation_result_result2 ]

    let path0 = Fpath.to_string (Fpath.v "ocaml/ocaml")
    let path1 = Fpath.to_string (Fpath.v "dinosaure/emile")
    let path2 = Fpath.to_string (Fpath.v "dinosaure/encore")
    let path3 = Fpath.to_string (Fpath.v "mirage/ocaml-git")

    let host0 = Some ("github.com", Some 80)
    let host1 = Some ("din.osau.re", None)
    let host2 = Some ("oktm.io", None)
    let host3 = Some ("mirage.io", Some 943)

    let git_proto_request0, git_proto_request_result0 =
      { Smart.Common.pathname = path0
      ; host = host0
      ; request_command = `Upload_pack }, `Equal

    let git_proto_request1, git_proto_request_result1 =
      { Smart.Common.pathname = path1
      ; host = host1
      ; request_command = `Upload_archive }, `Equal

    let git_proto_request2, git_proto_request_result2 =
      { Smart.Common.pathname = path2
      ; host = host2
      ; request_command = `Receive_pack }, `Equal

    let git_proto_request3, git_proto_request_result3 =
      { Smart.Common.pathname = path3
      ; host = host3
      ; request_command = `Upload_pack }, `Equal

    let git_proto_request_datas =
      [ git_proto_request0, git_proto_request_result0
      ; git_proto_request1, git_proto_request_result1
      ; git_proto_request2, git_proto_request_result2
      ; git_proto_request3, git_proto_request_result3 ]

    let zero =
      let s = String.make (S.Hash.Digest.length * 2) '0' in
      S.Hash.of_hex s

    let update_request0, update_request_result0 =
      let commands0 =
        let first = Smart.Common.Update (generate_hash (), generate_hash (), List.nth references 0) in
        let rest =
          [ Smart.Common.Create (zero, List.nth references 1) ] in
        `Raw (first, rest) in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; capabilities
      ; requests = commands0 }, `Error

    let update_request1, update_request_result1 =
      let commands1 =
        let first = Smart.Common.Update (generate_hash (), generate_hash (), List.nth references 1) in
        let rest =
          [ Smart.Common.Delete (zero, List.nth references 2) ] in
        `Raw (first, rest) in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; capabilities
      ; requests = commands1 }, `Error

    let update_request2, update_request_result2 =
      let commands2 =
        let first = Smart.Common.Update (zero, zero, List.nth references 2) in
        `Raw (first, []) in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; capabilities
      ; requests = commands2 }, `Error

    let update_request3, update_request_result3 =
      let commands3 =
        let first = Smart.Common.Update (generate_hash (), generate_hash (), List.nth references 3) in
        `Raw (first, []) in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; capabilities
      ; requests = commands3 }, `Equal

    let update_request4, update_request_result4 =
      let commands4 =
        let first = Smart.Common.Create (generate_hash (), List.nth references 4) in
        `Raw (first, []) in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; capabilities
      ; requests = commands4 }, `Equal

    let update_request5, update_request_result5 =
      let commands5 =
        let first = Smart.Common.Delete (generate_hash (), List.nth references 5) in
        `Raw (first, []) in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; capabilities
      ; requests = commands5 }, `Equal

    let update_request6, update_request_result6 =
      let commands6 =
        let first = Smart.Common.Update (generate_hash (), generate_hash (), List.nth references 6) in
        let rest =
          [ Smart.Common.Create (generate_hash (), List.nth references 7)
          ; Smart.Common.Delete (generate_hash (), List.nth references 8) ] in
        `Raw (first, rest) in
      { Smart.Common.shallow = generate_hashes (Random.int 10)
      ; capabilities
      ; requests = commands6 }, `Equal

    let update_request_datas =
      [ update_request0, update_request_result0
      ; update_request1, update_request_result1
      ; update_request2, update_request_result2
      ; update_request3, update_request_result3
      ; update_request4, update_request_result4
      ; update_request5, update_request_result5
      ; update_request6, update_request_result6 ]
  end

  let make_test
    : type v.
      name:string
    -> v
    -> v Smart.Decoder.transaction
    -> (v -> Smart.Encoder.action)
    -> [ `Equal | `Error ]
    -> (module Alcotest.TESTABLE with type t = v)
    -> unit Alcotest.test_case
    = fun ~name v trans to_action expect test ->
      name, `Quick,
      fun () ->
        let s = Smart.Encoder.to_string (to_action v) in

        match Smart.Decoder.of_string s trans, expect with
        | Ok v', `Equal -> Alcotest.check test name v v'
        | Ok v', `Error ->
          let module T = (val test) in

          (* XXX(dinosaure): it's possible to parse but diverge one results. *)
          if T.equal v v'
          then Alcotest.failf "Expected an errori (equality) with %s: %a" name T.pp v
        | Error _, `Error -> ()
        | Error (err, _, committed), `Equal ->
          Fmt.(pf stderr)
            "Input (committed: %02x): %a.\n"
            committed
            (Fmt.hvbox Encore.Lole.pp_string) s;
          Alcotest.failf "Got an error from the decoder: %a" Smart.Decoder.pp_error err

  type _ t =
    | Advertised_refs :
        (Smart.Common.advertised_refs * [ `Error | `Equal ]) list -> Smart.Common.advertised_refs t
    | Shallow_update :
        (Smart.Common.shallow_update * [ `Error | `Equal ]) list -> Smart.Common.shallow_update t
    | Acks :
        (Smart.Common.acks * S.Hash.t list * Smart.Decoder.ack_mode * [ `Error | `Equal ]) list -> Smart.Common.acks t
    | Negociation_result :
        (Smart.Common.negociation_result * [ `Error | `Equal ]) list -> Smart.Common.negociation_result t
    | Git_proto_request :
        (Smart.Common.git_proto_request * [ `Error | `Equal ]) list -> Smart.Common.git_proto_request t
    | Update_request :
        (Smart.Common.update_request * [ `Error | `Equal ]) list -> Smart.Common.update_request t

  let name_of : type v. v t -> string = function
    | Advertised_refs _ -> "advertised-refs"
    | Shallow_update _ -> "shallow-update"
    | Acks _ -> "acks"
    | Negociation_result _ -> "negociation-result"
    | Git_proto_request _ -> "git-proto-request"
    | Update_request _ -> "update-request"

  let testable_of
    : type v. v t -> (module Alcotest.TESTABLE with type t = v)
    = function
      | Advertised_refs _ ->
        let module T = struct
          type t = v

          let pp = Smart.Common.pp_advertised_refs
          let equal = Smart.Common.equal_advertised_refs
        end in (module T)
      | Shallow_update _ ->
        let module T = struct
          type t = v

          let pp = Smart.Common.pp_shallow_update
          let equal = Smart.Common.equal_shallow_update
        end in (module T)
      | Acks _ ->
        let module T = struct
          type t = v

          let pp = Smart.Common.pp_acks
          let equal = Smart.Common.equal_acks
        end in (module T)
      | Negociation_result _ ->
        let module T = struct
          type t = v

          let pp = Smart.Common.pp_negociation_result
          let equal = Smart.Common.equal_negociation_result
        end in (module T)
      | Git_proto_request _ ->
        let module T = struct
          type t = v

          let pp = Smart.Common.pp_git_proto_request
          let equal = Smart.Common.equal_git_proto_request
        end in (module T)
      | Update_request _ ->
        let module T = struct
          type t = v

          let pp = Smart.Common.pp_update_request
          let equal = Smart.Common.equal_update_request
        end in (module T)

  let from_t
    : type v. v t -> unit Alcotest.test_case list
    = fun t ->
      let name idx = Fmt.strf "%s %d" (name_of t) idx in
      let testable = testable_of t in
      match t with
      | Advertised_refs l ->
        List.mapi (fun idx (v, e) ->
            make_test
              ~name:(name idx)
              v
              Smart.Decoder.ReferenceDiscovery
              (fun v -> `Advertised_refs v)
              e testable) l
      | Shallow_update l ->
        List.mapi (fun idx (v, e) ->
            make_test
              ~name:(name idx)
              v
              Smart.Decoder.ShallowUpdate
              (fun v -> `Shallow_update v)
              e testable) l
      | Acks l ->
        List.mapi (fun idx (v, hashes, ack_mode, e) ->
            make_test
              ~name:(name idx)
              v
              Smart.Decoder.(Negociation (S.Hash.Set.of_list hashes, ack_mode))
              (fun v -> `Negociation v)
              e testable) l
      | Negociation_result l ->
        List.mapi (fun idx (v, e) ->
            make_test
              ~name:(name idx)
              v
              Smart.Decoder.NegociationResult
              (fun v -> `Negociation_result v)
              e testable) l
      | Git_proto_request l ->
        List.mapi (fun idx (v, e) ->
            make_test
              ~name:(name idx)
              v
              Smart.Decoder.Git_proto_request
              (fun v -> `GitProtoRequest v)
              e testable) l
      | Update_request l ->
        List.mapi (fun idx (v, e) ->
            make_test
              ~name:(name idx)
              v
              Smart.Decoder.Update_request
              (fun v -> `UpdateRequest v)
              e testable) l
end

let suite name (module S: Git.S) =
  let module T = Make(S) in
  name,
  (T.from_t (T.Advertised_refs T.Data.advertised_refs_datas)
   @ T.from_t (T.Shallow_update T.Data.shallow_update_datas)
   @ T.from_t (T.Acks T.Data.acks_datas)
   @ T.from_t (T.Negociation_result T.Data.negociation_result_datas)
   @ T.from_t (T.Git_proto_request T.Data.git_proto_request_datas)
   @ T.from_t (T.Update_request T.Data.update_request_datas))
