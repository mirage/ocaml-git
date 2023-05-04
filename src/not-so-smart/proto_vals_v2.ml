open Astring

module Proto_request = struct
  type t = {
    path : string;
    host : [ `host ] Domain_name.t * int option;
    version : int;
    request_command : [ `Upload_pack | `Receive_pack | `Upload_archive ];
  }

  let upload_pack ~host ?port ?(version = 2) path =
    let host = host, port in
    { request_command = `Upload_pack; host; version; path }

  let receive_pack ~host ?port ?(version = 1) path =
    let host = host, port in
    { request_command = `Receive_pack; host; version; path }

  let pp ppf { path; host; request_command; version } =
    let pp_request_command ppf = function
      | `Upload_pack -> Fmt.pf ppf "git-upload-pack"
      | `Receive_pack -> Fmt.pf ppf "git-receive-pack"
      | `Upload_archive -> Fmt.pf ppf "git-upload-archive"
    in
    let pp_host ppf = function
      | host, Some port -> Fmt.pf ppf "%a:%d" Domain_name.pp host port
      | host, None -> Fmt.pf ppf "%a" Domain_name.pp host
    in
    Fmt.pf ppf "%a %s %a %a" pp_request_command request_command path
      Fmt.((const string " host=") ++ pp_host)
      host
      Fmt.((const string " version=") ++ int)
      version
end

module Command = struct type t = { name : string; args : string list } end

module Ls_refs = struct
  type ref_attr =
    | Symref_target of string  (** symref target *)
    | Peeled of string  (** peeled obj-id *)

  type ref_ = { obj_id : string; name : string; attributes : ref_attr list }

  let pp_ref_attr ppf = function
    | Symref_target s -> Fmt.pf ppf "Symref-target %s" s
    | Peeled s -> Fmt.pf ppf "Peeled %s" s

  let pp_ref ppf { obj_id; name; attributes } =
    Fmt.pf ppf "{obj_id: %s;\n name: %s;\n attributes: [%a]}\n" obj_id name
      (Fmt.list pp_ref_attr) attributes

  type prefix = Prefix of string [@@unboxed]
  type request = { symrefs : bool; peel : bool; ref_prefixes : prefix list }
  type response = ref_ list

  let make_request ~symrefs ~peel ref_prefixes = { symrefs; peel; ref_prefixes }
end

module Fetch_command = struct
  type ack_res = Nak | Acks of string list

  type acks = {
    ack_res : ack_res option;
    is_ready : bool; (* false if ready line is absent *)
  }

  type response =
    | Acks_only of acks
    | Detailed_with_packfile of {
        acks : acks;
        shallow_info :
          [ `Shallows of string list ] * [ `Unshallows of string list ];
        wanted_refs : (string * string) list;
        packfile_uris : (string * string) list;
      }
end

module Extended_pkt_line_decoder = struct
  include Pkt_line.Decoder

  type error =
    [ Pkt_line.Decoder.error
    | `Mismatch of [ `Expected of err_constr ] * [ `Got of err_constr ] ]

  and err_constr =
    [ `Str of string
    | `Pkt of err_constr list
    | `Flush_pkt
    | `Delim_pkt
    | `Response_end_pkt
    | `Invalid_len_pkt of int
    | `Or of err_constr * err_constr ]

  let rec pp_err_constr ppf = function
    | `Str s -> Fmt.string ppf s
    | `Pkt lst ->
        Fmt.pf ppf "PKT-LINE(%a)" (Fmt.list ~sep:Fmt.nop pp_err_constr) lst
    | `Flush_pkt -> Fmt.string ppf "flush-pkt"
    | `Delim_pkt -> Fmt.string ppf "delim-pkt"
    | `Response_end_pkt -> Fmt.string ppf "Message pkt (aka response end pkt)"
    | `Invalid_len_pkt i -> Fmt.pf ppf "pkt of invalid length: %d" i
    | `Or (a, b) -> Fmt.pf ppf "(%a OR %a)" pp_err_constr a pp_err_constr b

  let mismatch ~expected ~got = `Mismatch (`Expected expected, `Got got)
  let unexpected_flush_pkt ~expected = mismatch ~expected ~got:`Flush_pkt
  let unexpected_delim_pkt ~expected = mismatch ~expected ~got:`Delim_pkt

  let unexpected_response_end_pkt ~expected =
    mismatch ~expected ~got:`Response_end_pkt

  let invalid_len_pkt ~expected l = mismatch ~expected ~got:(`Invalid_len_pkt l)

  let unexpected_pkt ~expected = function
    | Flush_pkt -> unexpected_flush_pkt ~expected
    | Delim_pkt -> unexpected_delim_pkt ~expected
    | Response_end_pkt -> unexpected_response_end_pkt ~expected
    | Invalid_len_pkt l -> invalid_len_pkt ~expected l
    | Pkt (_, pkt_content) -> mismatch ~expected ~got:(`Str pkt_content)

  let pp_error ppf = function
    | `Mismatch (`Expected exp, `Got got) ->
        Fmt.pf ppf "Expected: %a\nGot: %a\n" pp_err_constr exp pp_err_constr got
    | #Pkt_line.Decoder.error as err -> pp_error ppf err

  (** [skip_string s d] {!reads} a packet line from [d]
      and expects the read pkt line content to be equal to [s]

      @raise Invalid_argument if no packet line could be read *)
  let skip_string s decoder =
    match read_pkt decoder with
    | Pkt (_, s0) when String.equal s0 s -> return () decoder
    | Pkt (_, s0) -> Fmt.failwith "expected: %s\nfound: %s\n" s s0
    | _ -> invalid_arg "expected but didn't get a packet line"

  let error { buffer; pos; _ } error = Error { error; buffer; committed = pos }

  type ('acc, 'err, 'a, 'b) continue_or_stop =
    | Continue of 'acc
    | Stop_ok
    | Stop_ok_same_pos  (** keeps position [decoder.pos] same *)
    | Stop_err of 'err
        (** terminate decoding with error; keeps position [decoder.pos] same *)

  let continue acc = Continue acc
  let stop_ok = Stop_ok
  let stop_ok_same_pos = Stop_ok_same_pos
  let stop_err err = Stop_err err

  let decode_fold_until ~f ~init ~finalize decoder =
    let rec loop acc decoder =
      let pkt = peek_pkt' decoder in
      let pkt_len = pkt_len_at_least_4 pkt in
      let acc' = f acc pkt in
      match acc' with
      | Continue acc ->
          junk_chars pkt_len decoder;
          prompt_pkt (loop acc) decoder
      | Stop_ok_same_pos ->
          let res = finalize acc in
          return res decoder
      | Stop_ok ->
          junk_chars pkt_len decoder;
          let res = finalize acc in
          return res decoder
      | Stop_err err -> error decoder err
    in
    prompt_pkt (loop init) decoder
end

module Decoder = struct
  open Extended_pkt_line_decoder
  module Substr = String.Sub

  type nonrec error = error

  let pp_error = pp_error
  let ( >>=? ) x f = Option.bind x f
  let ( >|=? ) x f = Option.map f x

  (**
    capability-advertisement = protocol-version
                               capability-list
                               flush-pkt

    protocol-version = PKT-LINE("version 2" LF)
    capability-list = *capability
    capability = PKT-LINE(key[=value] LF)

    key = 1*(ALPHA | DIGIT | "-_")
    value = 1*(ALPHA | DIGIT | " -_.,?\/{}[]()<>!@#$%^&*+=:;") *)
  let decode_capability_ads decoder =
    (* protocol-version *)
    prompt_pkt (skip_string "version 2") decoder >>= fun () ->
    let expected = `Pkt [ `Str "key[=value] LF" ] in

    (* capability-list
       flush-pkt *)
    decode_fold_until decoder ~init:[] ~finalize:List.rev ~f:(fun acc ->
      function
      | Flush_pkt -> Stop_ok
      | Pkt (_, pkt_content) ->
          Continue (Capability.of_string pkt_content :: acc)
      | (Delim_pkt | Response_end_pkt | Invalid_len_pkt _) as pkt ->
          Stop_err (unexpected_pkt ~expected pkt))

  let v_space = Substr.of_string " "
  let v_colon = Substr.of_string ":"
  let is_symref_target_v s = Substr.equal s (Substr.of_string "symref-target")
  let is_peeled_v s = Substr.equal s (Substr.of_string "peeled")

  (**
    output = *ref
             flush-pkt

    ref = PKT-LINE(obj-id SP refname *(SP ref-attribute) LF)
    ref-attribute = (symref | peeled)
    symref = "symref-target:" symref-target
    peeled = "peeled:" obj-id *)
  let decode_ls_refs_response decoder =
    let expected =
      `Or (`Flush_pkt, `Pkt [ `Str "obj-id SP refname *(SP ref-attribute) LF" ])
    in
    (* ref-attribute *)
    let parse_ref_attr attr =
      Substr.cut ~sep:v_colon attr >>=? fun (k, v) ->
      match Substr.to_string k, Substr.to_string v with
      | "symref-target", v -> Some (Ls_refs.Symref_target v)
      | "peeled", v -> Some (Ls_refs.Peeled v)
      | _ -> None
    in
    (* ref *)
    let parse_ref ref_ =
      let s = String.Sub.of_string ref_ in
      match String.Sub.cuts ~sep:v_space s with
      | obj_id :: name :: ref_attrs ->
          let obj_id = Substr.to_string obj_id in
          let name = Substr.to_string name in
          let rec parse_or_none acc = function
            | [] -> Some (List.rev acc)
            | r :: rest ->
                parse_ref_attr r >>=? fun r -> parse_or_none (r :: acc) rest
          in
          parse_or_none [] ref_attrs
          |> Option.map (fun attributes -> Ls_refs.{ obj_id; name; attributes })
      | [] | _ :: _ -> None
    in
    decode_fold_until decoder ~init:[] ~finalize:List.rev ~f:(fun acc ->
      function
      | Flush_pkt -> Stop_ok
      | Pkt (_, pkt) -> (
          match parse_ref pkt with
          | Some ref_ -> Continue (ref_ :: acc)
          | None -> Stop_err (mismatch ~expected ~got:(`Str pkt)))
      | (Delim_pkt | Response_end_pkt | Invalid_len_pkt _) as pkt ->
          Stop_err (unexpected_pkt ~expected pkt))

  let peek_pkt ?(trim = true) decoder =
    let buf, off, len = peek_pkt decoder in
    let buf = Bytes.to_string buf in
    let res = String.Sub.v buf ~start:off ~stop:(off + len) in
    let is_new_line c = Char.equal c '\n' in
    if trim then String.Sub.trim ~drop:is_new_line res else res

  let prompt_pack_without_sideband kcontinue keof decoder =
    if decoder.pos > 0 then (
      let rest = decoder.max - decoder.pos in
      Bytes.unsafe_blit decoder.buffer decoder.pos decoder.buffer 0 rest;
      decoder.max <- rest;
      decoder.pos <- 0);
    let rec go off =
      if off = Bytes.length decoder.buffer && decoder.pos > 0 then
        Error
          {
            error = `No_enough_space;
            buffer = decoder.buffer;
            committed = decoder.pos;
          }
      else if off - decoder.pos > 0 then (
        decoder.max <- off;
        safe kcontinue decoder)
      else
        Read
          {
            buffer = decoder.buffer;
            off;
            len = Bytes.length decoder.buffer - off;
            continue = (fun len -> go (off + len));
            eof = keof decoder;
          }
    in
    go decoder.max

  let peek_pack_without_sideband (decoder : decoder) =
    let payload =
      Bytes.sub_string decoder.buffer decoder.pos (decoder.max - decoder.pos)
    in
    payload, 0, decoder.max - decoder.pos

  let junk_pack_without_sideband (decoder : decoder) =
    decoder.pos <- decoder.max

  let decode_pack ?(side_band = false) ~push_pack ~push_stdout ~push_stderr
      decoder =
    let with_side_band decoder =
      let v = peek_pkt ~trim:false decoder in
      match String.Sub.head v with
      | Some '\001' ->
          let off = String.Sub.start_pos v + 1 in
          let len = String.Sub.stop_pos v - off in
          let buf = String.Sub.base_string v in
          push_pack (buf, off, len);
          junk_pkt decoder;
          return true decoder
      | Some '\002' ->
          let tail = String.Sub.to_string (String.Sub.tail v) (* copy *) in
          push_stdout tail;
          junk_pkt decoder;
          return true decoder
      | Some '\003' ->
          let tail = String.Sub.to_string (String.Sub.tail v) (* copy *) in
          push_stderr tail;
          junk_pkt decoder;
          return true decoder
      | Some _ -> fail decoder (`Invalid_side_band (String.Sub.to_string v))
      | None -> return false decoder
    in
    let end_of_pack decoder () = return false decoder in
    let without_side_band decoder =
      let buf, off, len = peek_pack_without_sideband decoder in
      push_pack (buf, off, len);
      junk_pack_without_sideband decoder;
      return true decoder
    in
    if side_band then prompt_pkt ~strict:true with_side_band decoder
    else prompt_pack_without_sideband without_side_band end_of_pack decoder

  (** [if_str_else s then_ else_ d] peeks the to-be-read packet [p] and
      if its packet content equals [s], runs [then_] junking [p];
      otherwise, runs [else_] without junking packet [p]. *)
  let if_str_else str ~then_ ~else_ decoder =
    match peek_pkt' decoder with
    | Pkt (l, pkt_content) when String.equal pkt_content str ->
        junk_chars l decoder;
        prompt_pkt then_ decoder
    | Pkt _ | Flush_pkt | Delim_pkt | Response_end_pkt | Invalid_len_pkt _ ->
        prompt_pkt else_ decoder

  let or_delim_pkt other = `Or (`Delim_pkt, other)

  (**
    output = acknowledgements flush-pkt |
             [acknowledgments delim-pkt] [shallow-info delim-pkt]
             [wanted-refs delim-pkt] [packfile-uris delim-pkt]
             packfile flush-pkt

    acknowledgments = PKT-LINE("acknowledgments" LF)
		                  (nak | *ack)
                      (ready)
    Note: The spec for acknowledgements seem to confuse parens for brackets to
          specify "ready" as optional.

    ready = PKT-LINE("ready" LF)
    nak = PKT-LINE("NAK" LF)
    ack = PKT-LINE("ACK" SP obj-id LF)

    shallow-info = PKT-LINE("shallow-info" LF)
		               *PKT-LINE((shallow | unshallow) LF)
    shallow = "shallow" SP obj-id
    unshallow = "unshallow" SP obj-id

    wanted-refs = PKT-LINE("wanted-refs" LF)
		              *PKT-LINE(wanted-ref LF)
    wanted-ref = obj-id SP refname

    packfile-uris = PKT-LINE("packfile-uris" LF) *packfile-uri
    packfile-uri = PKT-LINE(40*(HEXDIGIT) SP *%x20-ff LF)

    packfile = PKT-LINE("packfile" LF)
	             *PKT-LINE(%x01-03 *%x00-ff)  *)
  let decode_fetch_response decoder =
    let open Fetch_command in
    let decode_detailed_with_packfile acks decoder =
      let decode_pack decoder : (unit, _) state =
        match read_pkt decoder with
        | Pkt (_, "packfile") -> failwith "(TODO:) not implemented"
        | _ as pkt ->
            unexpected_pkt ~expected:(`Str "packfile") pkt |> error decoder
      in

      let decode_packfile_uris decoder =
        let parse_packfile_uri s =
          String.cut ~sep:" " s >>=? fun (obj_id, v) ->
          if String.length obj_id = 40 then Some (obj_id, v) else None
        in
        let then_ decoder =
          let expected =
            or_delim_pkt (`Pkt [ `Str "40*(HEXDIGIT) SP *%x20-ff LF" ])
          in
          decode_fold_until decoder ~init:[] ~finalize:List.rev
            ~f:(fun acc pkt ->
              match pkt with
              | Delim_pkt -> Stop_ok
              | Pkt (_, pkt_content) -> (
                  match parse_packfile_uri pkt_content with
                  | None -> Stop_err (unexpected_pkt ~expected pkt)
                  | Some (obj_id, v) -> Continue ((obj_id, v) :: acc))
              | (Flush_pkt | Response_end_pkt | Invalid_len_pkt _) as pkt ->
                  Stop_err (unexpected_pkt ~expected pkt))
        in
        let else_ decoder = return [] decoder in
        prompt_pkt (if_str_else "packfile-uris" ~then_ ~else_) decoder
      in

      let decode_wanted_refs decoder =
        let then_ decoder =
          let expected = or_delim_pkt (`Pkt [ `Str "obj-id SP refname" ]) in
          decode_fold_until decoder ~init:[] ~finalize:List.rev
            ~f:(fun acc pkt ->
              match pkt with
              | Delim_pkt -> Stop_ok
              | Pkt (_, pkt_content) -> (
                  match String.cut ?rev:None ~sep:" " pkt_content with
                  | Some (obj_id, refname) when String.length obj_id = 40 ->
                      Continue ((obj_id, refname) :: acc)
                  | Some _ | None -> Stop_err (unexpected_pkt ~expected pkt))
              | Flush_pkt | Response_end_pkt | Invalid_len_pkt _ ->
                  Stop_err (unexpected_pkt ~expected pkt))
        in
        let else_ decoder = return [] decoder in
        prompt_pkt (if_str_else "wanted-refs" ~then_ ~else_) decoder
      in

      let decode_shallow_info decoder =
        let then_ decoder =
          let expected =
            `Or
              ( `Delim_pkt,
                `Or
                  ( `Pkt [ `Str "\"shallow\" SP obj-id" ],
                    `Pkt [ `Str "\"unshallow\" SP obj-id" ] ) )
          in
          decode_fold_until decoder ~init:([], [])
            ~finalize:(fun (ll, lr) ->
              `Shallows (List.rev ll), `Unshallows (List.rev lr))
            ~f:(fun (shallows, unshallows) pkt ->
              match pkt with
              | Delim_pkt -> Stop_ok
              | Pkt (_, pkt_content) -> (
                  match String.cut ~sep:" " pkt_content with
                  | Some ("shallow", obj_id) ->
                      Continue (obj_id :: shallows, unshallows)
                  | Some ("unshallow", obj_id) ->
                      Continue (shallows, obj_id :: unshallows)
                  | None | Some _ -> Stop_err (unexpected_pkt ~expected pkt))
              | Flush_pkt | Response_end_pkt | Invalid_len_pkt _ ->
                  Stop_err (unexpected_pkt ~expected pkt))
        in
        let else_ decoder = return (`Shallows [], `Unshallows []) decoder in
        prompt_pkt (if_str_else "shallow-info" ~then_ ~else_) decoder
      in

      prompt_pkt decode_shallow_info decoder >>= fun shallow_info ->
      prompt_pkt decode_wanted_refs decoder >>= fun wanted_refs ->
      prompt_pkt decode_packfile_uris decoder >>= fun packfile_uris ->
      prompt_pkt decode_pack decoder >>= fun () ->
      return
        (Detailed_with_packfile
           { acks; shallow_info; wanted_refs; packfile_uris })
        decoder
    in

    (* acknowledgements *)
    let decode_acknowledgements decoder =
      let decode_acks_flush_or_delim ~is_ready nak_or_acks decoder =
        match read_pkt decoder with
        | Flush_pkt ->
            return (Acks_only { ack_res = nak_or_acks; is_ready }) decoder
        | Delim_pkt ->
            prompt_pkt
              (decode_detailed_with_packfile
                 { ack_res = nak_or_acks; is_ready })
              decoder
        | _ -> failwith "expected flush-pkt or delim-pkt"
      in

      let decode_ready ~is_ready nak_or_acks decoder =
        if is_ready then
          prompt_pkt (decode_acks_flush_or_delim ~is_ready nak_or_acks) decoder
        else
          match peek_pkt' decoder with
          | Flush_pkt | Delim_pkt ->
              decode_acks_flush_or_delim ~is_ready:false nak_or_acks decoder
          | Response_end_pkt | Invalid_len_pkt _ ->
              failwith "was trying to parse ready"
          | Pkt (l, "ready") ->
              junk_chars l decoder;
              prompt_pkt
                (decode_acks_flush_or_delim ~is_ready:true nak_or_acks)
                decoder
          | Pkt _ -> failwith "unexpected string %s"
      in

      let rec decode_acks acks decoder =
        match peek_pkt' decoder with
        | Flush_pkt | Delim_pkt ->
            decode_acks_flush_or_delim ~is_ready:false (Some (Acks acks))
              decoder
        | Pkt (l, "ready") ->
            junk_chars l decoder;
            let acks = match acks with [] -> None | _ -> Some (Acks acks) in
            prompt_pkt (decode_ready ~is_ready:true acks) decoder
        | Pkt (l, pkt) -> (
            match String.cut ~sep:" " pkt with
            | None -> failwith "was decoding acks but got %s"
            | Some ("ACK", obj_id) ->
                junk_chars l decoder;
                prompt_pkt (decode_acks (obj_id :: acks)) decoder
            | Some _ -> failwith "unexpected string")
        | Response_end_pkt | Invalid_len_pkt _ -> failwith "was decoding acks"
      in

      prompt_pkt (skip_string "acknowledgements") decoder >>= fun () ->
      let k decoder =
        match peek_pkt' decoder with
        | Flush_pkt | Delim_pkt ->
            (* don't need [prompt_pkt] because we peeked and saw pkt available *)
            decode_acks_flush_or_delim ~is_ready:false None decoder
        | Pkt (l, "NAK") ->
            junk_chars l decoder;
            prompt_pkt (decode_ready ~is_ready:false (Some Nak)) decoder
        | Pkt (l, "ready") ->
            junk_chars l decoder;
            prompt_pkt (decode_acks_flush_or_delim ~is_ready:true None) decoder
        | Pkt (_, pkt) when String.is_prefix ~affix:"ACK " pkt ->
            decode_acks [] decoder
        | (Response_end_pkt | Invalid_len_pkt _ | Pkt _) as pkt ->
            unexpected_pkt
              ~expected:(`Or (`Str "(ready)", `Str "(nak | *ack)"))
              pkt
            |> error decoder
      in
      prompt_pkt k decoder
    in
    decode_acknowledgements decoder
end

module Encoder = struct
  open Pkt_line.Encoder

  type nonrec error = error

  let pp_error = pp_error
  let kdone _encoder = Done

  let kflush encoder =
    write encoder "0000";
    flush kdone encoder

  let kdelim_pkt encoder = write encoder "0001"
  let write_space encoder = write encoder " "
  let write_zero encoder = write encoder "\000"
  let write_lf encoder = write encoder "\n"

  (* different from [delayed_write_pkt] defined in [nss/protocol] in that
     pkt lines are appended by LF as instructed in the git specs *)
  let delayed_write_pkt k0 k1 ({ pos; payload } as encoder) =
    (* leave space for pkt length: 4 bytes *)
    encoder.pos <- pos + 4;
    k0 encoder;
    write_lf encoder;
    (* XXX(dinosaure): or [encoder.pos <- encoder.pos + 4]? *)
    let len = encoder.pos - pos in
    Bytes.blit_string (Fmt.str "%04X" len) 0 payload pos 4;
    flush k1 encoder

  let encode_flush = kflush

  let encode_proto_request encoder
      { Proto_request.path; host; version; request_command } =
    let write_request_command encoder = function
      | `Upload_pack -> write encoder "git-upload-pack"
      | `Receive_pack -> write encoder "git-receive-pack"
      | `Upload_archive -> write encoder "git-upload-archive"
    in
    let write_version encoder version =
      let version = Fmt.str "version=%d" version in
      write encoder version
    in
    let write_host encoder = function
      | host, Some port ->
          let host = Fmt.str "host=%s:%d" (Domain_name.to_string host) port in
          write encoder host
      | host, None ->
          let host = Fmt.str "host=%s" (Domain_name.to_string host) in
          write encoder host
    in
    let k encoder =
      write_request_command encoder request_command;
      write_space encoder;
      write encoder path;
      write_zero encoder;
      write_host encoder host;
      write_zero encoder;
      if version > 1 then (
        write_zero encoder;
        write_version encoder version;
        write_zero encoder)
    in
    delayed_write_pkt k kdone encoder

  (**
    request = empty-request | command-request
    empty-request = flush-pkt
    command-request = command
                      capability-list
                      [command-args]
                      flush-pkt
    command = PKT-LINE("command=" key LF)
    command-args = delim-pkt
		               *command-specific-arg

    command-specific-args are packet line framed arguments defined by
    each individual command.  *)
  let encode_request req capabilities encoder =
    match req with
    | `Empty -> kflush encoder
    | `Command Command.{ name; args } ->
        (* command-args *)
        let write_command_args args encoder =
          match args with
          | [] -> kflush encoder
          | args ->
              let rec loop args encoder =
                match args with
                | [] -> kflush encoder
                | arg :: rest ->
                    let write_arg encoder = write encoder arg in
                    delayed_write_pkt write_arg (loop rest) encoder
              in
              delayed_write_pkt kdelim_pkt (loop args) encoder
        in
        (* capability-list *)
        let rec write_caps caps encoder =
          match caps with
          | [] -> write_command_args args encoder
          | hd :: tl ->
              let write_cap encoder = write encoder (Capability.to_string hd) in
              delayed_write_pkt write_cap (write_caps tl) encoder
        in
        (* command *)
        let write_command encoder =
          write encoder @@ Fmt.str "command=%s" name
        in
        delayed_write_pkt write_command (write_caps capabilities) encoder

  let ls_refs_request_args { Ls_refs.symrefs; peel; ref_prefixes } =
    let ref_pref_args = List.map (fun (Ls_refs.Prefix p) -> p) ref_prefixes in
    let peel_arg = if peel then [ "peel" ] else [] in
    let symrefs_arg = if symrefs then [ "symrefs" ] else [] in
    List.concat
      [
        symrefs_arg; peel_arg; ref_pref_args;
        (* order of args placement may matter *)
      ]

  let encode_ls_refs_request capabilities encoder req =
    let args = ls_refs_request_args req in
    let command = `Command { Command.name = "ls-refs"; args } in
    encode_request command capabilities encoder
end
