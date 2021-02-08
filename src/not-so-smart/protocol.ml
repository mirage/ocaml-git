let ( <.> ) f g x = f (g x)

module Advertised_refs = struct
  type ('uid, 'reference) t = {
    shallows : 'uid list;
    refs : ('uid * 'reference * bool) list;
    capabilities : Capability.t list;
    version : int;
  }

  let equal_shallows ~uid:equal_uid l0 l1 =
    if List.length l0 <> List.length l1 then false
    else List.for_all (fun uid0 -> List.exists (equal_uid uid0) l1) l0

  let equal_advertised_refs ~uid:equal_uid ~reference:equal_reference l0 l1 =
    if List.length l0 <> List.length l1 then false
    else
      List.for_all
        (fun (uid0, ref0, peeled0) ->
          List.exists
            (fun (uid1, ref1, peeled1) ->
              equal_uid uid0 uid1
              && equal_reference ref0 ref1
              && peeled0 = peeled1)
            l1)
        l0

  let equal_capabilities l0 l1 =
    if List.length l0 <> List.length l1 then false
    else List.for_all (fun c0 -> List.exists (Capability.equal c0) l1) l0

  let equal ~uid:equal_uid ~reference:equal_reference a b =
    equal_shallows ~uid:equal_uid a.shallows b.shallows
    && equal_advertised_refs ~uid:equal_uid ~reference:equal_reference a.refs
         b.refs
    && equal_capabilities a.capabilities b.capabilities
    && a.version = b.version

  let head { refs; _ } =
    try
      let uid, _, _ =
        List.find (function _, "HEAD", false -> true | _ -> false) refs
      in
      Some uid
    with _exn -> None

  let reference ~equal ?(peeled = false) refname { refs; _ } =
    try
      let uid, _, _ =
        List.find
          (fun (_, refname', peeled') ->
            equal refname refname' && peeled = peeled')
          refs
      in
      Some uid
    with _exn -> None

  let references ~equal ?(peeled = false) refnames { refs; _ } =
    let fold acc (uid, refname', peeled') =
      if List.exists (equal refname') refnames && peeled = peeled' then
        uid :: acc
      else acc
    in
    List.fold_left fold [] refs

  let refs { refs; _ } = refs
  let capabilities { capabilities; _ } = capabilities

  let map ~fuid ~fref { shallows; refs; capabilities; version } =
    let shallows = List.map fuid shallows in
    let refs =
      List.map (fun (uid, ref, peeled) -> fuid uid, fref ref, peeled) refs
    in
    { shallows; refs; capabilities; version }

  let pp ppf { shallows; refs; capabilities; version } =
    Fmt.pf ppf "version %d@ " version;
    match refs with
    | [] ->
        Fmt.pf ppf "0 capabilities^{}@ ";
        Fmt.pf ppf "%a@," Fmt.(Dump.list Capability.pp) capabilities;
        List.iter (Fmt.pf ppf "shallow %s@ ") shallows
    | head :: refs ->
        let pp_ref ppf (uid, refname, peeled) =
          if peeled then Fmt.pf ppf "%s %s^{}" uid refname
          else Fmt.pf ppf "%s %s" uid refname
        in
        Fmt.pf ppf "%a@ " pp_ref head;
        Fmt.pf ppf "%a@ " Fmt.(Dump.list Capability.pp) capabilities;
        List.iter (Fmt.pf ppf "%a@ " pp_ref) refs;
        List.iter (Fmt.pf ppf "shallow %s@ ") shallows

  let v1 ?(shallows = []) ?(capabilities = []) refs =
    { shallows; capabilities; refs; version = 1 }
end

module Proto_request = struct
  type t = {
    path : string;
    host :
      [ `Addr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ] * int option;
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
      | `Domain host, Some port -> Fmt.pf ppf "%a:%d" Domain_name.pp host port
      | `Domain host, None -> Fmt.pf ppf "%a" Domain_name.pp host
      | `Addr v, Some port -> Fmt.pf ppf "%a:%d" Ipaddr.pp v port
      | `Addr v, None -> Ipaddr.pp ppf v
    in
    Fmt.pf ppf "%a %s %a %a" pp_request_command request_command path
      Fmt.(prefix (const string " host=") pp_host)
      host
      Fmt.(prefix (const string " version=") int)
      version
end

module Want = struct
  type ('uid, 'reference) t = {
    wants : 'uid * 'uid list;
    shallows : 'uid list;
    deepen :
      [ `Depth of int | `Timestamp of int64 | `Not of 'reference ] option;
    filter : Filter.t option;
    capabilities : Capability.t list;
  }

  let want ~capabilities ?deepen ?filter ?(shallows = []) ?(others = []) hash =
    { wants = hash, others; shallows; deepen; filter; capabilities }
end

module Result = struct
  type 'uid t = NAK | ACK of 'uid

  let pp ppf = function
    | NAK -> Fmt.pf ppf "NAK"
    | ACK common -> Fmt.pf ppf "ACK %s" common
end

module Negotiation = struct
  type 'uid t =
    | ACK of 'uid
    | ACK_continue of 'uid
    | ACK_ready of 'uid
    | ACK_common of 'uid
    | NAK

  let is_common = function ACK_common _ -> true | _ -> false
  let is_ready = function ACK_ready _ -> true | _ -> false
  let is_nak = function NAK -> true | _ -> false

  let pp ppf = function
    | ACK uid -> Fmt.pf ppf "ACK %s" uid
    | ACK_continue uid -> Fmt.pf ppf "ACK %s continue" uid
    | ACK_ready uid -> Fmt.pf ppf "ACK %s ready" uid
    | ACK_common uid -> Fmt.pf ppf "ACK %s common" uid
    | NAK -> Fmt.pf ppf "NAK"

  let map ~f = function
    | ACK uid -> ACK (f uid)
    | ACK_continue uid -> ACK_continue (f uid)
    | ACK_ready uid -> ACK_ready (f uid)
    | ACK_common uid -> ACK_common (f uid)
    | NAK -> NAK
end

module Commands = struct
  type ('uid, 'ref) command =
    | Create of 'uid * 'ref
    | Delete of 'uid * 'ref
    | Update of 'uid * 'uid * 'ref

  let map_command ~fuid ~fref = function
    | Create (uid, ref) -> Create (fuid uid, fref ref)
    | Delete (uid, ref) -> Delete (fuid uid, fref ref)
    | Update (a, b, ref) -> Update (fuid a, fuid b, fref ref)

  type ('uid, 'ref) t = {
    capabilities : Capability.t list;
    commands : ('uid, 'ref) command * ('uid, 'ref) command list;
  }

  let create uid reference = Create (uid, reference)
  let delete uid reference = Delete (uid, reference)
  let update a b reference = Update (a, b, reference)

  let v ~capabilities ?(others = []) command =
    { capabilities; commands = command, others }

  let commands { commands = command, others; _ } = command :: others

  let map ~fuid ~fref { commands = command, others; capabilities } =
    let command = map_command ~fuid ~fref command in
    let others = List.map (map_command ~fuid ~fref) others in
    { commands = command, others; capabilities }
end

module Shallow = struct
  type 'uid t = Shallow of 'uid | Unshallow of 'uid

  let map ~f = function
    | Shallow v -> Shallow (f v)
    | Unshallow v -> Unshallow (f v)
end

module Status = struct
  type 'ref t = {
    result : (unit, string) result;
    commands : ('ref, 'ref * string) result list;
  }

  let pp ppf { result; commands } =
    let ok_with_ref ppf ref = Fmt.pf ppf "%s:ok" ref in
    let error_with_ref ppf (ref, err) = Fmt.pf ppf "%s:%s" ref err in
    Fmt.pf ppf "{ @[<hov>result= %a;@ commands= @[<hov>%a@];@] }"
      Fmt.(Dump.result ~ok:(const string "done") ~error:string)
      result
      Fmt.(Dump.list (Dump.result ~ok:ok_with_ref ~error:error_with_ref))
      commands

  let to_result { result; _ } = result

  let map ~f { result; commands } =
    let commands =
      let fold = function
        | Ok ref -> Ok (f ref)
        | Error (ref, err) -> Error (f ref, err)
      in
      List.map fold commands
    in
    { result; commands }

  let v ?(err = "An error occurred") cmds =
    let commands =
      let map = function
        | Ok (Commands.Create (_, ref))
        | Ok (Commands.Delete (_, ref))
        | Ok (Commands.Update (_, _, ref)) ->
            Ok ref
        | Error
            ( ( Commands.Create (_, ref)
              | Commands.Delete (_, ref)
              | Commands.Update (_, _, ref) ),
              err ) ->
            Error (ref, err)
      in
      List.map map cmds
    in
    if List.exists Rresult.R.is_error commands then
      { result = Error err; commands }
    else { result = Ok (); commands }
end

module Decoder = struct
  open Astring
  open Pkt_line.Decoder

  type nonrec error =
    [ error
    | `Invalid_advertised_ref of string
    | `Invalid_shallow of string
    | `Invalid_negotiation_result of string
    | `Invalid_side_band of string
    | `Invalid_ack of string
    | `Invalid_result of string
    | `Invalid_command_result of string
    | `Unexpected_flush
    | `Invalid_pkt_line ]

  let pp_error ppf = function
    | #Pkt_line.Decoder.error as err -> Pkt_line.Decoder.pp_error ppf err
    | `Invalid_advertised_ref raw ->
        Fmt.pf ppf "Invalid advertised refererence (%S)" raw
    | `Invalid_shallow raw -> Fmt.pf ppf "Invalid shallow (%S)" raw
    | `Invalid_negotiation_result raw ->
        Fmt.pf ppf "Invalid negotiation result (%S)" raw
    | `Invalid_side_band raw -> Fmt.pf ppf "Invalid side-band (%S)" raw
    | `Invalid_ack raw -> Fmt.pf ppf "Invalid ack (%S)" raw
    | `Invalid_result raw -> Fmt.pf ppf "Invalid result (%S)" raw
    | `Invalid_command_result raw ->
        Fmt.pf ppf "Invalid result command (%S)" raw
    | `Unexpected_flush -> Fmt.string ppf "Unexpected flush"

  let is_new_line = function '\n' -> true | _ -> false

  let peek_pkt ?(trim = true) decoder =
    let buf, off, len = peek_pkt decoder in
    let buf = Bytes.to_string buf in
    let res = String.Sub.v buf ~start:off ~stop:(off + len) in
    if trim then String.Sub.trim ~drop:is_new_line res else res

  let is_zero = function '0' -> true | _ -> false
  let v_zero = String.Sub.of_string "\000"
  let v_space = String.Sub.of_string " "
  let v_peeled = String.Sub.of_string "^{}"
  let v_shallow = String.Sub.of_string "shallow"
  let v_unshallow = String.Sub.of_string "unshallow"
  let v_version = String.Sub.of_string "version"
  let v_nak = String.Sub.of_string "NAK"
  let v_ack = String.Sub.of_string "ACK"
  let v_ok = String.Sub.of_string "ok"
  let v_ng = String.Sub.of_string "ng"

  let decode_advertised_refs decoder =
    let decode_shallows advertised_refs decoder =
      let rec go shallows decoder =
        let v = peek_pkt decoder in
        if String.Sub.is_empty v then (
          junk_pkt decoder;
          return { advertised_refs with Advertised_refs.shallows } decoder)
        else
          match String.Sub.cut ~sep:v_space v with
          | Some (_, uid) ->
              let uid = String.Sub.to_string uid in
              junk_pkt decoder;
              let k decoder = go (uid :: shallows) decoder in
              prompt_pkt k decoder
          | None -> fail decoder (`Invalid_shallow (String.Sub.to_string v))
      in
      go [] decoder
    in
    (* obj-id refname *)
    let decode_others_refs ~version ~head ~capabilities decoder =
      let rec go refs decoder =
        let v = peek_pkt decoder in
        if String.Sub.is_empty v then (
          junk_pkt decoder;
          return
            {
              Advertised_refs.capabilities;
              refs = List.rev refs;
              version;
              shallows = [];
            }
            decoder)
        else if String.Sub.is_prefix ~affix:v_shallow v then
          decode_shallows
            {
              Advertised_refs.capabilities;
              refs = List.rev refs;
              version;
              shallows = [];
            }
            decoder
        else
          match String.Sub.cut ~sep:v_space v with
          | Some (uid, reference) ->
              let uid = String.Sub.to_string uid in
              let reference, peeled =
                match String.Sub.cut ~rev:true ~sep:v_peeled reference with
                | Some (reference, _) -> String.Sub.to_string reference, true
                | None -> String.Sub.to_string reference, false
              in
              let k decoder = go ((uid, reference, peeled) :: refs) decoder in
              junk_pkt decoder;
              prompt_pkt k decoder
          | None ->
              fail decoder (`Invalid_advertised_ref (String.Sub.to_string v))
      in
      go [ head ] decoder
    in

    (* zero-id capabilities^{}\000capabilities *)
    let decode_no_ref ~version v decoder =
      let _, rest = Option.get (String.Sub.cut ~sep:v_space v) in
      match String.Sub.cut ~sep:v_zero rest with
      | Some (_, capabilities) ->
          let capabilities = String.Sub.fields capabilities in
          let capabilities =
            List.map
              (Capability.of_string <.> String.Sub.to_string)
              capabilities
          in
          junk_pkt decoder;
          return
            { Advertised_refs.capabilities; refs = []; version; shallows = [] }
            decoder
      | None -> fail decoder (`Invalid_advertised_ref (String.Sub.to_string v))
    in

    (* obj-id HEAD\000capabilities *)
    let decode_first_ref ~version v decoder =
      let uid, rest = Option.get (String.Sub.cut ~sep:v_space v) in
      match String.Sub.cut ~sep:v_zero rest with
      | Some (head, capabilities) ->
          let uid = String.Sub.to_string uid in
          let capabilities = String.Sub.fields capabilities in
          let capabilities =
            List.map
              (Capability.of_string <.> String.Sub.to_string)
              capabilities
          in
          let peeled = String.Sub.is_suffix ~affix:v_peeled head in
          let head =
            if peeled then
              String.Sub.with_range ~len:(String.Sub.length head - 3) head
            else head
          in
          let head = String.Sub.to_string head in
          junk_pkt decoder;
          let k decoder =
            decode_others_refs ~version ~head:(uid, head, peeled) ~capabilities
              decoder
          in
          prompt_pkt k decoder
      | None -> fail decoder (`Invalid_advertised_ref (String.Sub.to_string v))
    in

    (* zero-id capabilities^{}\000capabilities
       | obj-id HEAD\000capabilities *)
    let decode_refs ?(version = 1) decoder =
      let v = peek_pkt decoder in
      match String.Sub.cut ~sep:v_space v with
      | Some (uid, _) ->
          if String.Sub.for_all is_zero uid then
            decode_no_ref ~version v decoder
          else decode_first_ref ~version v decoder
      | None ->
          (* XXX(dinosaure): see [empty_clone]. *)
          junk_pkt decoder;
          return
            {
              Advertised_refs.shallows = [];
              Advertised_refs.refs = [];
              Advertised_refs.capabilities = [];
              Advertised_refs.version = 1;
            }
            decoder
    in

    (* version (1|2) *)
    let decode_version decoder =
      let v = peek_pkt decoder in
      if String.Sub.is_prefix ~affix:v_version v then
        match String.Sub.cut ~sep:v_space v with
        | Some (_, version) ->
            let version = int_of_string (String.Sub.to_string version) in
            junk_pkt decoder;
            prompt_pkt (decode_refs ~version) decoder
        | None -> decode_refs ~version:1 decoder
      else decode_refs decoder
    in

    (* only for HTTP *)
    let rec decode_comment ?(comment = false) decoder =
      let v = peek_pkt decoder in
      match String.Sub.head v with
      | Some '#' ->
          junk_pkt decoder;
          prompt_pkt (decode_comment ~comment:true) decoder
      | Some _ -> decode_version decoder
      | None ->
          (* XXX(dinosaure): HTTP starts with a comment AND [0000]. We must
           * consume it to correctly parse advertised refs then. However,
           * for an empty clone (over TCP), we must not consume it. *)
          if comment then junk_pkt decoder;
          prompt_pkt decode_version decoder
    in

    prompt_pkt decode_comment decoder

  let decode_result decoder =
    let k decoder =
      let v = peek_pkt decoder in
      if String.Sub.equal_bytes v v_nak then (
        junk_pkt decoder;
        return Result.NAK decoder)
      else
        match String.Sub.cut ~sep:v_space v with
        | Some (_, common) ->
            let common = String.Sub.to_string common in
            junk_pkt decoder;
            return (Result.ACK common) decoder
        | None ->
            fail decoder (`Invalid_negotiation_result (String.Sub.to_string v))
    in
    prompt_pkt k decoder

  let decode_packet ~trim decoder =
    let k decoder =
      let v = peek_pkt ~trim decoder in
      let r = String.Sub.to_string v in
      junk_pkt decoder;
      return r decoder
    in
    prompt_pkt k decoder

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

  let decode_shallows decoder =
    let rec go acc decoder =
      let v = peek_pkt decoder in
      if String.Sub.length v = 0 then (
        junk_pkt decoder;
        return (List.rev acc) decoder)
      else if
        String.Sub.is_prefix ~affix:v_shallow v
        || String.Sub.is_prefix ~affix:v_unshallow v
      then
        match String.Sub.cut ~sep:v_space v with
        | Some (v, uid) ->
            let uid = String.Sub.to_string uid in
            if String.Sub.equal_bytes v v_shallow then (
              junk_pkt decoder;
              prompt_pkt (go (Shallow.Shallow uid :: acc)) decoder)
            else (
              junk_pkt decoder;
              prompt_pkt (go (Shallow.Unshallow uid :: acc)) decoder)
        | _ -> return (List.rev acc) decoder
      else return (List.rev acc) decoder
    in
    prompt_pkt (go []) decoder

  let decode_negotiation decoder =
    let k decoder =
      let pkt = peek_pkt decoder in
      if String.Sub.equal_bytes pkt v_nak then (
        junk_pkt decoder;
        return Negotiation.NAK decoder)
      else if String.Sub.is_prefix ~affix:v_ack pkt then
        match String.Sub.cuts ~sep:v_space pkt with
        | [ _; uid ] ->
            let uid = String.Sub.to_string uid in
            junk_pkt decoder;
            return (Negotiation.ACK uid) decoder
        | [ _; uid; v ] -> (
            let uid = String.Sub.to_string uid in
            match
              let v = String.Sub.to_string v in
              junk_pkt decoder;
              v
            with
            | "continue" -> return (Negotiation.ACK_continue uid) decoder
            | "ready" -> return (Negotiation.ACK_ready uid) decoder
            | "common" -> return (Negotiation.ACK_common uid) decoder
            | _ -> fail decoder (`Invalid_ack (String.Sub.to_string pkt)))
        | _ -> fail decoder (`Invalid_ack (String.Sub.to_string pkt))
      else assert false
    in
    prompt_pkt k decoder

  let rec bind x ~f =
    match x with
    | Done v -> f v
    | Read { buffer; off; len; continue; eof } ->
        let continue len = bind (continue len) ~f in
        let eof () = bind (eof ()) ~f in
        Read { buffer; off; len; continue; eof }
    | Error _ as err -> err

  let ( >>= ) x f = bind x ~f

  let decode_status decoder =
    let command pkt =
      match String.Sub.cuts ~sep:v_space pkt with
      | res :: reference :: rest -> (
          match String.Sub.to_string res with
          | "ok" -> Stdlib.Ok (Stdlib.Ok (String.Sub.to_string reference))
          | "ng" ->
              let err = String.Sub.(to_string (concat ~sep:v_space rest)) in
              let reference = String.Sub.to_string reference in
              Stdlib.Ok (Stdlib.Error (reference, err))
          | _ ->
              Stdlib.Error (`Invalid_command_result (String.Sub.to_string pkt)))
      | _ -> Stdlib.Error (`Invalid_command_result (String.Sub.to_string pkt))
    in

    let commands decoder =
      let rec go acc decoder =
        let pkt = peek_pkt decoder in
        if String.Sub.length pkt = 0 then return (List.rev acc) decoder
        else if
          String.Sub.is_prefix ~affix:v_ok pkt
          || String.Sub.is_prefix ~affix:v_ng pkt
        then
          match command pkt with
          | Ok x ->
              junk_pkt decoder;
              prompt_pkt (go (x :: acc)) decoder
          | Error err -> fail decoder err
        else fail decoder (`Invalid_command_result (String.Sub.to_string pkt))
      in
      prompt_pkt (go []) decoder
    in

    let result decoder =
      let pkt = peek_pkt decoder in
      match String.Sub.cut ~sep:v_space pkt with
      | None -> fail decoder (`Invalid_result (String.Sub.to_string pkt))
      | Some (_unpack, res) -> (
          match String.Sub.(to_string (trim res)) with
          | "ok" ->
              junk_pkt decoder;
              return (Stdlib.Ok ()) decoder
          | err ->
              junk_pkt decoder;
              return (Stdlib.Error err) decoder)
    in
    prompt_pkt result decoder >>= fun result ->
    prompt_pkt commands decoder >>= fun commands ->
    return { Status.result; Status.commands } decoder

  (* XXX(dinosaure): even if we handle with and without sideband, currently the
     default [decode_status] parse a sideband. On the Irmin side, sideband is
     used in any case but we should improve [protocol.ml] and pass true
     [sideband] value. *)

  let decode_status ?(sideband = true) decoder =
    let with_sideband decoder =
      let pkt = peek_pkt decoder in
      match String.Sub.head pkt with
      | Some '\001' ->
          let str = String.Sub.(to_string (tail pkt)) in
          let decoder' = of_string str in
          decode_status decoder' >>= fun res ->
          junk_pkt decoder;
          prompt_pkt (return res) decoder
      | Some _ -> assert false (* TODO *)
      | None ->
          junk_pkt decoder;
          return { Status.result = Ok (); Status.commands = [] } decoder
    in
    let without_sideband decoder =
      let pkt = peek_pkt decoder in
      if String.Sub.length pkt <> 0 then
        fail decoder (`Invalid_command_result (String.Sub.to_string pkt))
      else (
        junk_pkt decoder;
        return { Status.result = Ok (); Status.commands = [] } decoder)
    in
    if sideband then prompt_pkt with_sideband decoder
    else prompt_pkt without_sideband decoder
end

module Encoder = struct
  open Pkt_line.Encoder

  type nonrec error = error

  let pp_error = pp_error
  let write_space encoder = write encoder " "
  let write_zero encoder = write encoder "\000"
  let write_new_line encoder = write encoder "\n"

  let delayed_write_pkt k0 k1 ({ pos; payload } as encoder) =
    (* leave space for pkt length: 4 bytes *)
    encoder.pos <- pos + 4;
    k0 encoder;
    (* XXX(dinosaure): or [encoder.pos <- encoder.pos + 4]? *)
    let len = encoder.pos - pos in
    Bytes.blit_string (Fmt.str "%04X" len) 0 payload pos 4;
    flush k1 encoder

  let kdone _encoder = Done

  let kflush encoder =
    write encoder "0000";
    flush kdone encoder

  let encode_flush encoder = kflush encoder

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
    let pp_host ppf = function
      | `Domain v -> Domain_name.pp ppf v
      | `Addr v -> Ipaddr.pp ppf v
    in
    let write_host encoder = function
      | host, Some port ->
          let host = Fmt.str "host=%a:%d" pp_host host port in
          write encoder host
      | host, None ->
          let host = Fmt.str "host=%a" pp_host host in
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

  let encode_want encoder
      { Want.capabilities; wants = first, others; shallows; deepen; filter } =
    let filter encoder =
      match filter with Some _ -> . | None -> encode_flush encoder
    in

    let deepen encoder =
      match deepen with
      | None -> filter encoder
      | Some (`Depth depth) ->
          let depth encoder =
            write encoder "deepen";
            write_space encoder;
            write encoder (string_of_int depth)
          in
          delayed_write_pkt depth filter encoder
      | Some (`Timestamp timestamp) ->
          let timestamp encoder =
            write encoder "deepen-since";
            write_space encoder;
            write encoder (Int64.to_string timestamp)
          in
          delayed_write_pkt timestamp filter encoder
      | Some (`Not reference) ->
          let not encoder =
            write encoder "deepen-not";
            write_space encoder;
            write encoder reference
          in
          delayed_write_pkt not filter encoder
    in

    let shallows encoder =
      let shallow hash encoder =
        write encoder "shallow";
        write_space encoder;
        write encoder hash
      in
      let rec go shallows encoder =
        match shallows with
        | [] -> deepen encoder
        | head :: tail -> delayed_write_pkt (shallow head) (go tail) encoder
      in
      go shallows encoder
    in

    let others encoder =
      let want hash encoder =
        write encoder "want";
        write_space encoder;
        write encoder hash
      in
      let rec go others encoder =
        match others with
        | [] -> shallows encoder
        | head :: tail -> delayed_write_pkt (want head) (go tail) encoder
      in
      go others encoder
    in

    let first encoder =
      write encoder "want";
      write_space encoder;
      write encoder first;
      let rec go = function
        | [] -> ()
        | [ capability ] -> write encoder (Capability.to_string capability)
        | head :: tail ->
            write encoder (Capability.to_string head);
            write_space encoder;
            go tail
      in
      if List.length capabilities > 0 then (
        write_space encoder;
        go capabilities);
      write_new_line encoder
    in

    delayed_write_pkt first others encoder

  let encode_done encoder =
    let k encoder =
      write encoder "done";
      write_new_line encoder
    in
    delayed_write_pkt k kdone encoder

  let unsafe_encode_packet ({ pos; payload; _ } as encoder) ~packet =
    encoder.pos <- pos + 4;
    write encoder packet;
    let len = encoder.pos - pos in
    Bytes.blit_string (Fmt.str "%04X" len) 0 payload pos 4

  let write_command encoder = function
    | Commands.Create (uid, r) ->
        let zero_id = String.make (String.length uid) '0' in
        write encoder zero_id;
        write_space encoder;
        write encoder uid;
        write_space encoder;
        write encoder r
    | Commands.Delete (uid, r) ->
        let zero_id = String.make (String.length uid) '0' in
        write encoder uid;
        write_space encoder;
        write encoder zero_id;
        write_space encoder;
        write encoder r
    | Commands.Update (a, b, r) ->
        write encoder a;
        write_space encoder;
        write encoder b;
        write_space encoder;
        write encoder r

  let encode_commands encoder
      { Commands.capabilities; commands = first, others } =
    let others encoder =
      let command c encoder = write_command encoder c in
      let rec go others encoder =
        match others with
        | [] -> kflush encoder
        | head :: tail -> delayed_write_pkt (command head) (go tail) encoder
      in
      go others encoder
    in
    let first encoder =
      write_command encoder first;
      let rec go = function
        | [] -> ()
        | [ capability ] -> write encoder (Capability.to_string capability)
        | head :: tail ->
            write encoder (Capability.to_string head);
            write_space encoder;
            go tail
      in
      write_zero encoder;
      if List.length capabilities > 0 then go capabilities
    in
    delayed_write_pkt first others encoder

  let encode_advertised_refs encoder advertised_refs =
    let encode_shallows shallows encoder =
      let encode_shallow shallow encoder =
        write encoder "shallow";
        write_space encoder;
        write encoder shallow
      in
      let rec go shallows encoder =
        match shallows with
        | [] -> kflush encoder
        | hd :: tl -> delayed_write_pkt (encode_shallow hd) (go tl) encoder
      in
      go shallows encoder
    in
    let encode_others_refs others encoder =
      let encode_advertised_ref uid refname peeled encoder =
        write encoder uid;
        write_space encoder;
        write encoder refname;
        if peeled then write encoder "^{}"
      in
      let rec go others encoder =
        match others with
        | [] -> encode_shallows advertised_refs.Advertised_refs.shallows encoder
        | (uid, refname, peeled) :: rest ->
            delayed_write_pkt
              (encode_advertised_ref uid refname peeled)
              (go rest) encoder
      in
      go others encoder
    in
    let encode_first_ref (uid, refname, peeled) encoder =
      write encoder uid;
      write_space encoder;
      write encoder refname;
      if peeled then write encoder "^{}";
      write_zero encoder;
      let rec go = function
        | [] -> ()
        | [ capability ] -> write encoder (Capability.to_string capability)
        | head :: tail ->
            write encoder (Capability.to_string head);
            write_space encoder;
            go tail
      in
      go advertised_refs.Advertised_refs.capabilities
    in
    let encode_no_refs encoder =
      let capabilities = "capabilities^{}" and zero_uid = String.make 40 '0' in
      write encoder zero_uid;
      write_space encoder;
      write encoder capabilities;
      write_zero encoder;
      let rec go = function
        | [] -> ()
        | [ capability ] -> write encoder (Capability.to_string capability)
        | head :: tail ->
            write encoder (Capability.to_string head);
            write_space encoder;
            go tail
      in
      go advertised_refs.Advertised_refs.capabilities
    in
    match advertised_refs.Advertised_refs.refs with
    | (uid, refname, peeled) :: others ->
        delayed_write_pkt
          (encode_first_ref (uid, refname, peeled))
          (encode_others_refs others)
          encoder
    | [] ->
        delayed_write_pkt encode_no_refs
          (encode_shallows advertised_refs.Advertised_refs.shallows)
          encoder

  (* TODO(dinosaure): handle HTTP/stateless and side-band. *)
  let encode_pack ?side_band:(_ = false) ?stateless:(_ = false) encoder payload
      =
    let rec go buffer off max encoder =
      if max = 0 then flush kdone encoder
      else
        let len = min max (Bytes.length encoder.payload - encoder.pos) in
        Bytes.blit_string payload off encoder.payload encoder.pos len;
        encoder.pos <- encoder.pos + len;
        flush (go buffer (off + len) (max - len)) encoder
    in
    go payload 0 (String.length payload) encoder
end
