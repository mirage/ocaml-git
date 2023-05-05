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
    host : string * int option;
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
      | host, Some port -> Fmt.pf ppf "%s:%d" host port
      | host, None -> Fmt.string ppf host
    in
    Fmt.pf ppf "%a %s %a %a" pp_request_command request_command path
      Fmt.(const string " host=" ++ pp_host)
      host
      Fmt.(const string " version=" ++ int)
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

  let v ~capabilities ?deepen ?filter ?(shallows = []) = function
    | [] -> invalid_arg "Smart.Want.v: you must specify, at least, one hash"
    | hd :: tl -> { wants = hd, tl; shallows; deepen; filter; capabilities }

  let pp_deepen ppf = function
    | `Depth n -> Fmt.pf ppf "@[<1>(`Depth %d)@]" n
    | `Timestamp v -> Fmt.pf ppf "@[<1>(`Timestamp %Ld)@]" v
    | `Not reference -> Fmt.pf ppf "@[<1>(`Not %S)@]" reference

  let equal_deepen ~reference a b =
    match a, b with
    | Some (`Depth a), Some (`Depth b) -> a = b
    | Some (`Timestamp a), Some (`Timestamp b) -> a = b
    | Some (`Not a), Some (`Not b) -> reference a b
    | None, None -> true
    | _ -> false

  let equal_filter : Filter.t option -> Filter.t option -> bool =
   fun a b ->
    match a, b with None, None -> true | Some _, _ -> . | _, Some _ -> .

  let pp ppf { wants; shallows; deepen; filter; capabilities } =
    Fmt.pf ppf
      "@[<hov>{ wants=@[<hov>%a@];@ shallows=@[<hov>%a@];@ \
       deepen=@[<hov>%a@];@ filter=@[<hov>%a@];@ capabilities=@[<hov>%a@]; }@]"
      Fmt.(Dump.list string)
      (fst wants :: snd wants)
      Fmt.(Dump.list string)
      shallows
      Fmt.(Dump.option pp_deepen)
      deepen
      Fmt.(Dump.option Filter.pp)
      filter
      Fmt.(Dump.list Capability.pp)
      capabilities

  let equal ~uid ~reference a b =
    uid (fst a.wants) (fst b.wants)
    && List.for_all2 uid (snd a.wants) (snd b.wants)
    && List.for_all2 uid a.shallows b.shallows
    && equal_deepen ~reference a.deepen b.deepen
    && equal_filter a.filter b.filter
    && List.for_all2 Capability.equal
         (List.sort Capability.compare a.capabilities)
         (List.sort Capability.compare b.capabilities)
end

module Have = struct
  type 'uid t = 'uid list * [ `Done | `Flush ]

  let have ~cmd haves = haves, cmd
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

  let is_common = function ACK_common _ -> true | _ -> false
  let is_ready = function ACK_ready _ -> true | _ -> false

  let pp ppf = function
    | ACK uid -> Fmt.pf ppf "ACK %s" uid
    | ACK_continue uid -> Fmt.pf ppf "ACK %s continue" uid
    | ACK_ready uid -> Fmt.pf ppf "ACK %s ready" uid
    | ACK_common uid -> Fmt.pf ppf "ACK %s common" uid

  let map ~f = function
    | ACK uid -> ACK (f uid)
    | ACK_continue uid -> ACK_continue (f uid)
    | ACK_ready uid -> ACK_ready (f uid)
    | ACK_common uid -> ACK_common (f uid)
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

  let capabilities { capabilities; _ } = capabilities

  let pp_command pp_uid pp_ref ppf = function
    | Create (uid, r) ->
        Fmt.pf ppf "@[<1>(Create@ @[<1>(%a,@ %a)@])@]" pp_uid uid pp_ref r
    | Delete (uid, r) ->
        Fmt.pf ppf "@[<1>(Delete@ @[<1>(%a,@ %a)@])@]" pp_uid uid pp_ref r
    | Update (a, b, r) ->
        Fmt.pf ppf "@[<1>(Update@ @[<1>(%a,@ %a,@ %a)@])@]" pp_uid a pp_uid b
          pp_ref r

  let pp pp_uid pp_ref ppf { capabilities; commands } =
    Fmt.pf ppf "{ @[<hov>capabilities= @[<hov>%a@];@ commands= @[<hov>%a@];@] }"
      Fmt.(Dump.list Capability.pp)
      capabilities
      Fmt.(
        Dump.pair (pp_command pp_uid pp_ref)
          Dump.(list (pp_command pp_uid pp_ref)))
      commands

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
    commands : [ `FF of 'ref | `OK of 'ref | `ER of 'ref * string ] list;
  }

  let pp_commands pp_ref ppf = function
    | `FF reference -> Fmt.pf ppf "fast-forward %a" pp_ref reference
    | `OK reference -> Fmt.pf ppf "ok %a" pp_ref reference
    | `ER (reference, err) -> Fmt.pf ppf "error on %a: %s" pp_ref reference err

  let pp ppf { result; commands } =
    Fmt.pf ppf "{ @[<hov>result= %a;@ commands= @[<hov>%a@];@] }"
      Fmt.(Dump.result ~ok:(const string "done") ~error:string)
      result
      Fmt.(Dump.list (pp_commands string))
      commands

  let to_result { result; _ } = result

  let map ~f { result; commands } =
    let commands =
      let fold = function
        | `FF ref -> `FF (f ref)
        | `OK ref -> `OK (f ref)
        | `ER (ref, err) -> `ER (f ref, err)
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
            `OK ref
        | Error
            ( ( Commands.Create (_, ref)
              | Commands.Delete (_, ref)
              | Commands.Update (_, _, ref) ),
              err ) ->
            `ER (ref, err)
      in
      List.map map cmds
    in
    if List.exists (function `ER _ -> true | _ -> false) commands then
      { result = Error err; commands }
    else { result = Ok (); commands }
end

module Decoder = struct
  open Astring
  open Pkt_line.Decoder
  module Sub = String.Sub

  type nonrec error =
    [ error
    | `Invalid_advertised_ref of string
    | `Invalid_shallow of string
    | `Invalid_negotiation_result of string
    | `Invalid_side_band of string
    | `Invalid_ack of string
    | `Invalid_result of string
    | `Invalid_command_result of string
    | `Invalid_command of string
    | `Invalid_want of string
    | `Invalid_have of string
    | `Unexpected_flush
    | `Unexpected_pkt_line of string
    | `Invalid_pkt_line of string ]

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
    | `Invalid_want raw -> Fmt.pf ppf "Invalid want (%S)" raw
    | `Invalid_have raw -> Fmt.pf ppf "Invalid have (%S)" raw
    | `Unexpected_flush -> Fmt.string ppf "Unexpected flush"
    | `Unexpected_pkt_line raw -> Fmt.pf ppf "Unexpected pkt-line (%S)" raw
    | `Invalid_command cmd -> Fmt.pf ppf "Invalid command (%S)" cmd

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
  let v_done = String.Sub.of_string "done"
  let v_have = String.Sub.of_string "have"
  let v_want = String.Sub.of_string "want "

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
          let k decoder =
            let pkt = peek_pkt decoder in
            if String.Sub.length pkt = 0 then (
              junk_pkt decoder;
              return
                {
                  Advertised_refs.capabilities;
                  refs = [];
                  version;
                  shallows = [];
                }
                decoder)
            else
              fail decoder (`Invalid_advertised_ref (String.Sub.to_string pkt))
          in
          prompt_pkt k decoder
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

  let decode_have decoder =
    let rec go haves decoder =
      let v = peek_pkt decoder in
      if Sub.is_empty v then (
        junk_pkt decoder;
        return (Have.have ~cmd:`Flush (List.rev haves)) decoder)
      else if Sub.equal_bytes v_done v then (
        junk_pkt decoder;
        return (Have.have ~cmd:`Done (List.rev haves)) decoder)
      else
        match Sub.cut ~sep:v_space v with
        | Some (have, new_have) ->
            let haves =
              if Sub.equal_bytes have v_have then
                Sub.to_string new_have :: haves
              else haves
            in
            let k decoder = go haves decoder in
            junk_pkt decoder;
            prompt_pkt k decoder
        | _ -> fail decoder (`Invalid_have (Sub.to_string v))
    in
    prompt_pkt (go []) decoder

  let decode_want decoder =
    let decode_all_wants ~first_want ~capabilities decoder =
      let rec go wants decoder =
        let v = peek_pkt decoder in
        if Sub.is_empty v then (
          junk_pkt decoder;
          return
            (Some (Want.v ~capabilities (first_want :: List.rev wants)))
            decoder
          (* TODO else if start with shallow or depth request or filter request then *))
        else
          match Sub.cut ~sep:v_space v with
          | Some (_, new_want) ->
              let new_want = Sub.to_string new_want in
              let k decoder = go (new_want :: wants) decoder in
              junk_pkt decoder;
              prompt_pkt k decoder
          | None -> fail decoder (`Invalid_want (Sub.to_string v))
      in
      go [] decoder
    in

    let decode_first_want decoder =
      let v = peek_pkt decoder in
      if Sub.is_empty v then return None decoder
      else if Sub.is_prefix v ~affix:v_want then (
        let v = v |> Sub.with_range ~first:(Sub.length v_want) in
        (* NOTE(dinosaure): we accept more than Git. The BNF syntax of
           [first-want] is:
           first-want := PKT-LINE("want" SP obj-id SP capability-list)

           Here, we allow the client to pass 0 capabilities where Git expects,
           at least, one. *)
        match Sub.cut ~sep:v_space v with
        | None ->
            let first_want = Sub.to_string v in
            junk_pkt decoder;
            let k = decode_all_wants ~first_want ~capabilities:[] in
            prompt_pkt k decoder
        | Some (first_want, capabilities) ->
            let first_want = Sub.to_string first_want in
            junk_pkt decoder;
            let capabilities =
              let capabilities = Sub.fields capabilities in
              List.map (Capability.of_string <.> Sub.to_string) capabilities
            in
            let k = decode_all_wants ~first_want ~capabilities in
            prompt_pkt k decoder)
      else fail decoder (`Invalid_want (Sub.to_string v))
    in
    prompt_pkt decode_first_want decoder

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

  let decode_pack ?(side_band = false) ~push_stdout ~push_stderr decoder =
    let with_side_band decoder =
      let v = peek_pkt ~trim:false decoder in
      match String.Sub.head v with
      | Some '\001' ->
          let str = String.Sub.to_string (String.Sub.tail v) in
          junk_pkt decoder;
          return (`Payload (str, 0, String.length str)) decoder
      | Some '\002' ->
          let tail = String.Sub.to_string (String.Sub.tail v) (* copy *) in
          push_stdout tail;
          junk_pkt decoder;
          return `Stdout decoder
      | Some '\003' ->
          let tail = String.Sub.to_string (String.Sub.tail v) (* copy *) in
          push_stderr tail;
          junk_pkt decoder;
          return `Stderr decoder
      | Some _ -> fail decoder (`Invalid_side_band (String.Sub.to_string v))
      | None -> return `End_of_transmission decoder
    in
    let end_of_pack decoder () = return `End_of_transmission decoder in
    let without_side_band decoder =
      let buf, off, len = peek_pack_without_sideband decoder in
      junk_pack_without_sideband decoder;
      return (`Payload (buf, off, len)) decoder
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
    let rec k decoder =
      let pkt = peek_pkt decoder in
      if String.Sub.equal_bytes pkt v_nak then (
        junk_pkt decoder;
        return `NAK decoder)
      else if String.Sub.is_prefix ~affix:v_ack pkt then
        match String.Sub.cuts ~sep:v_space pkt with
        | [ _; uid ] ->
            let uid = String.Sub.to_string uid in
            junk_pkt decoder;
            return (`ACK (Negotiation.ACK uid)) decoder
        | [ _; uid; v ] -> (
            let uid = String.Sub.to_string uid in
            match
              let v = String.Sub.to_string v in
              junk_pkt decoder;
              v
            with
            | "continue" -> return (`ACK (Negotiation.ACK_continue uid)) decoder
            | "ready" -> return (`ACK (Negotiation.ACK_ready uid)) decoder
            | "common" -> return (`ACK (Negotiation.ACK_common uid)) decoder
            | _ -> fail decoder (`Invalid_ack (String.Sub.to_string pkt)))
        | _ -> fail decoder (`Invalid_ack (String.Sub.to_string pkt))
      else (
        junk_pkt decoder;
        prompt_pkt k decoder)
    in
    prompt_pkt k decoder

  let decode_flush decoder =
    let k decoder =
      let pkt = peek_pkt decoder in
      if String.Sub.length pkt = 0 then return () decoder
      else fail decoder (`Unexpected_pkt_line (String.Sub.to_string pkt))
    in
    prompt_pkt k decoder

  let decode_status decoder =
    let command decoder =
      let pkt = peek_pkt decoder in
      if String.Sub.length pkt = 0 then Stdlib.Ok None
      else
        match String.Sub.cuts ~sep:v_space pkt with
        | res :: reference :: rest -> (
            match String.Sub.to_string res with
            | "ok" -> Stdlib.Ok (Some (`OK (String.Sub.to_string reference)))
            | "ng" ->
                let err = String.Sub.(to_string (concat ~sep:v_space rest)) in
                let reference = String.Sub.to_string reference in
                Stdlib.Ok (Some (`ER (reference, err)))
            | "ff" -> Stdlib.Ok (Some (`FF (String.Sub.to_string reference)))
            | _ ->
                Stdlib.Error
                  (`Invalid_command_result (String.Sub.to_string pkt)))
        | _ -> Stdlib.Error (`Invalid_command_result (String.Sub.to_string pkt))
    in

    let commands res decoder =
      let rec go acc decoder =
        match command decoder with
        | Ok (Some x) ->
            junk_pkt decoder;
            prompt_pkt (go (x :: acc)) decoder
        | Ok None ->
            return { Status.result = res; commands = List.rev acc } decoder
        | Error err -> fail decoder err
      in
      go [] decoder
    in

    let result decoder =
      let pkt = peek_pkt decoder in
      match String.Sub.cut ~sep:v_space pkt with
      | None -> return { Status.result = Stdlib.Ok (); commands = [] } decoder
      | Some (_unpack, res) -> (
          match String.Sub.(to_string (trim res)) with
          | "ok" ->
              junk_pkt decoder;
              prompt_pkt (commands (Stdlib.Ok ())) decoder
          | err ->
              junk_pkt decoder;
              prompt_pkt (commands (Stdlib.Error err)) decoder)
    in

    prompt_pkt result decoder

  let decode_status ?(sideband = true) decoder =
    match sideband with
    | true ->
        let rec go buf decoder =
          let pkt = peek_pkt ~trim:false decoder in
          match String.Sub.head pkt with
          | Some '\001' ->
              let str = String.Sub.(to_string (tail pkt)) in
              Buffer.add_string buf str;
              junk_pkt decoder;
              prompt_pkt (go buf) decoder
          | Some _ ->
              junk_pkt decoder;
              prompt_pkt (go buf) decoder
          | None ->
              let decoder' = of_string (Buffer.contents buf) in
              decode_status decoder'
        in
        prompt_pkt (go (Buffer.create 0x100)) decoder
    | false -> decode_status decoder

  let decode_commands decoder =
    let rec rest_commands ({ Commands.commands = first, rest; _ } as res)
        decoder =
      let pkt = peek_pkt decoder in
      match String.Sub.cuts ~sep:v_space pkt with
      | [ oid0; oid1; reference ] ->
          let reference = String.Sub.to_string reference in
          let commands =
            match
              String.Sub.for_all is_zero oid0, String.Sub.for_all is_zero oid1
            with
            | true, false ->
                ( first,
                  Commands.Create (String.Sub.to_string oid1, reference) :: rest
                )
            | false, true ->
                ( first,
                  Commands.Delete (String.Sub.to_string oid0, reference) :: rest
                )
            | false, false ->
                ( first,
                  Commands.Update
                    ( String.Sub.to_string oid0,
                      String.Sub.to_string oid1,
                      reference )
                  :: rest )
            | _ -> assert false
          in
          junk_pkt decoder;
          prompt_pkt (rest_commands { res with commands }) decoder
      | _ -> return (Some res) decoder
    in
    let first_command decoder =
      let pkt = peek_pkt decoder in
      match String.Sub.cut ~sep:v_zero pkt with
      | Some (cmd, capabilities) -> (
          let capabilities = String.Sub.fields capabilities in
          let capabilities =
            List.map
              (Capability.of_string <.> String.Sub.to_string)
              capabilities
          in
          match String.Sub.cuts ~sep:v_space cmd with
          | [ oid0; oid1; reference ] ->
              let reference = String.Sub.to_string reference in
              let commands =
                match
                  ( String.Sub.for_all is_zero oid0,
                    String.Sub.for_all is_zero oid1 )
                with
                | true, false ->
                    Commands.Create (String.Sub.to_string oid1, reference), []
                | false, true ->
                    Commands.Delete (String.Sub.to_string oid0, reference), []
                | false, false ->
                    ( Commands.Update
                        ( String.Sub.to_string oid0,
                          String.Sub.to_string oid1,
                          reference ),
                      [] )
                | true, true -> assert false
              in
              junk_pkt decoder;
              prompt_pkt
                (rest_commands { Commands.capabilities; commands })
                decoder
          | _ -> fail decoder (`Invalid_command (String.Sub.to_string cmd)))
      | None ->
          junk_pkt decoder;
          return None decoder
    in
    prompt_pkt first_command decoder
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
    let write_host encoder = function
      | host, Some port ->
          let host = Fmt.str "host=%s:%d" host port in
          write encoder host
      | host, None ->
          let host = Fmt.str "host=%s" host in
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
        if peeled then write encoder "^{}";
        write_new_line encoder
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
        | [ capability ] ->
            write encoder (Capability.to_string capability);
            write_new_line encoder
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

  let encode_acks encoder acks =
    (* TODO: Remove NACK from [Negotiation.t]. *)
    let write_nak encoder = write encoder "NAK" in
    let write_ack ack encoder =
      let write_ack uid suffix =
        write encoder "ACK";
        write_space encoder;
        write encoder uid;
        (match suffix with
        | None -> ()
        | Some s ->
            write_space encoder;
            write encoder s);
        write_new_line encoder
      in
      match ack with
      | Negotiation.ACK uid -> write_ack uid None
      | ACK_continue uid -> write_ack uid (Some "continue")
      | ACK_ready uid -> write_ack uid (Some "ready")
      | ACK_common uid -> write_ack uid (Some "common")
    in
    let rec go acks encoder =
      match acks with
      | [] -> delayed_write_pkt write_nak (flush kdone) encoder
      | hd :: tl -> delayed_write_pkt (write_ack hd) (go tl) encoder
    in
    go acks encoder
end
