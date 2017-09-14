(* XXX(dinosaure): this comment is to understand the choice to use Angstrom
   instead a decoder like the Smart decoder or the PACK file. Indeed, in the
   Smart decoder, I explained the choice to make a new kind of decoder to
   de-serialize the stream.

   But for the Smart HTTP protocol, we have two problem:
   * The protocol is not strictly the same as the Smart protocol
   * This protocol is in top of the HTTP protocol, which takes care about some details

   So for this two reasons, it's better to diverge from the Smart decoder (and
   create a new specific decoder for the Smart HTTP protocol) and use the good
   Angstrom abstraction to de-serialize the flow. We have the {i pkt-line}
   format in the Smart HTTP protocol like the Smart protocol but the HTTP
   protocol (the sub-layer) already takes care about the length of the stream
   firstly, take cares about some information and handle the authentication
   (contrary to the Smart protocol which need to be wrapped in a SSH tunnel).

   So, because the context of the Smart HTTP protocol is not the same than the
   Smart protocol, it's better to re-implement all and provide a mandatory
   different API than [Sync].

   For the re-usability, it's better to use Angstrom instead a local-defined
   decoder. Indeed, in the Smart protocol, we decided to make our own decoder
   (in the same way than [ocaml-imap]) because we can take the advantage of the
   {i pkt-line} format to parse the flow.

   In the Smart HTTP protocol, we handle the {i pkt-line} too but in top of the
   HTTP protocol and try to use the advantage of the {i pkt-line} is a non-sense
   because the advantage (length of the flow) is already the purpose of the
   sub-layer HTTP protocol. *)

module Decoder
    (H : S.HASH with type hex = string)
    (W : S.WEB with type +'a io = 'a Lwt.t
                and type raw = Cstruct.buffer) =
struct
  module Hash = H
  module Web = W

  let hexdigit =
    let open Angstrom in

    peek_char_fail >>= function
    | '0' .. '9' as chr -> take 1 *> return (Char.code chr - 48)
    | 'a' .. 'f' as chr -> take 1 *> return (Char.code chr - 97 + 10)
    | 'A' .. 'F' as chr -> take 1 *> return (Char.code chr - 65 + 10)
    | _ -> fail "Invalid hexadecimal value"

  let pkt_header =
    let open Angstrom in

    hexdigit >>= fun a ->
    hexdigit >>= fun b ->
    hexdigit >>= fun c ->
    hexdigit >>| fun d ->
    (a * (16 * 16 * 16)) + (b * (16 * 16)) + (c * 16) + d

  let pkt_line p =
    let open Angstrom in

    pkt_header >>= fun n ->
    take (n - 4) >>= fun s ->
    match parse_string p s with
    | Ok x -> return x
    | Error x -> fail x

  let pkt_empty =
    let open Angstrom in
    string "0000" >>= fun _ -> return ()

  let is_not_lf = function '\n' -> false | _ -> true
  let is_not_nul = function '\000' -> false | _ -> true
  let is_not_caret = function '^' -> false | _ -> true
  let is_not_sp_and_lf = function ' ' | '\n' -> false | _ -> true

  let hash =
    let open Angstrom in
    take (Hash.Digest.length * 2) >>| Hash.of_hex

  let zero_id =
    let open Angstrom in
    let zero_id = String.make (Hash.Digest.length * 2) '0' in
    string zero_id >>| fun _ -> ()

  let first_line ?service () =
    let open Angstrom in

    let p service =
      string "# service=" *> take_while is_not_lf >>| fun value -> match service with
      | Some service when String.equal service value -> true
      | Some _ -> false
      | None -> true
    in

    pkt_line (p service)

  type advertised_refs =
    { refs         : (Hash.t * string * Hash.t option) list
    ; capabilities : Capability.t list }

  let pp_advertised_refs ppf { refs; capabilities; } =
    let sep = Fmt.unit ";@ " in
    let pp_ref ppf (hash, refname, peeled) =
      match peeled with
      | Some peeled -> Fmt.pf ppf "%a %s and %a" (Fmt.hvbox Hash.pp) hash refname (Fmt.hvbox Hash.pp) peeled
      | None -> Fmt.pf ppf "%a %s" (Fmt.hvbox Hash.pp) hash refname
    in

    Fmt.pf ppf "{ @[<hov>refs = [ %a ];@ \
                capabilites = [ %a ];@] }"
      (Fmt.hvbox @@ Fmt.list ~sep (Fmt.hvbox pp_ref)) refs
      (Fmt.hvbox @@ Fmt.list ~sep Capability.pp_capability) capabilities
  (* XXX(dinosaure): OK, this is the same as the Smart protocol with a strongly
     constraint about peeled reference. Then, the shallow information is not
     available. *)

  let sp = Angstrom.char ' '
  let lf = Angstrom.char '\n'
  let nul = Angstrom.char '\000'

  let capability =
    let open Angstrom in

    let is = function
      | 'a' .. 'z'
      | 'A' .. 'Z'
      | '0' .. '9'
      | '_' | '-' -> true
      | _ -> false
    in

    take_while is >>= fun capability -> peek_char >>= function
    | Some '=' ->
      take 1 *> take_while is_not_sp_and_lf >>| fun value ->
      Capability.of_string ~value capability
    | Some _ | None ->
      return (Capability.of_string capability)

  let cap_list =
    let open Angstrom in
    sep_by1 sp capability

  let empty_list =
    let open Angstrom in

    let p =
      zero_id *> sp *> string "capabilities^{}" *> nul *> cap_list <* lf >>| fun capabilities ->
      { refs = []
      ; capabilities }
    in

    pkt_line p

  let any_ref =
    let open Angstrom in
    pkt_line (hash <* sp >>= fun hash -> take_while is_not_lf <* lf >>| fun name -> (hash, name))

  let peeled_ref =
    let open Angstrom in

    any_ref >>= fun ref0 ->
    (pkt_line (hash <* sp >>= fun hash ->
               take_while is_not_caret <* string "^{}" <* lf >>| fun name ->
               (hash, name))) >>= fun ref1 ->
    let (hash0, name0) = ref0 in
    let (hash1, name1) = ref1 in

    if String.equal name0 name1
    then return (hash0, hash1, name0)
    else fail "Invalid peeled reference"

  let ref_record =
    let open Angstrom in

    (peeled_ref >>| fun (hash0, hash1, name) -> (hash0, name, Some hash1))
    <|> (any_ref >>| fun (hash, name) -> (hash, name, None))

  let non_empty_list =
    let open Angstrom in

    let p =
      hash <* sp >>= fun hash ->
      take_while is_not_nul <* nul >>= fun name ->
      cap_list <* lf >>| fun capabilities ->
      { refs = [ (hash, name, None) ]
      ; capabilities }
    in

    (pkt_line p) >>= fun x -> many ref_record >>| fun rest -> { x with refs = x.refs @ rest }

  let ref_list =
    let open Angstrom in
    non_empty_list <|> empty_list

  let smart_reply ?service () =
    let open Angstrom in

    first_line ?service () >>= function
    | true ->
      pkt_empty *> ref_list <* pkt_empty
    (* XXX(dinosaure): the first empty line is not described in the
       documentation but it exists in the current implementation. *)
    | false -> fail "Invalid service"

  let empty = Cstruct.create 0 |> Cstruct.to_bigarray

  let nak =
    Angstrom.(pkt_line (string "NAK" *> lf) >>= fun _ -> available >>= take)

  let decode response ?(status = W.s200_ok) p =
    let open Lwt.Infix in

    if W.Response.status response = status
    then
      let get = W.Response.body response in

      let rec go = function
        | Angstrom.Unbuffered.Done (_, value) -> Lwt.return (Ok value)
        | Angstrom.Unbuffered.Fail (_, path, err) -> Lwt.return (Error err)
        | Angstrom.Unbuffered.Jump jump -> (go[@tailcall]) (jump ())
        | Angstrom.Unbuffered.Partial { Angstrom.Unbuffered.committed; continue; } ->
          get () >>= function
          | None -> continue empty Angstrom.Unbuffered.Complete |> go
          | Some (raw, off, len) ->
            continue (Bigarray.Array1.sub raw off len) Angstrom.Unbuffered.Incomplete |> go
      in

      go (Angstrom.Unbuffered.parse p)
    else Lwt.return (Error "Invalid status code")
end

module Encoder
    (H : S.HASH with type hex = string)
    (W : S.WEB with type +'a io = 'a Lwt.t
                and type raw = Cstruct.buffer) =
struct
  module Hash = H
  module Web = W

  type upload_request =
    { want         : Hash.t * Hash.t list
    ; have         : Hash.t list
    ; capabilities : Capability.t list }

  let pkt_header = Bytes.create 4

  let pkt_line kk k encoder =
    let open Minienc in

    let write_header shift encoder =
      let has = shift in

      if has > 0xFFFF
      then raise (Invalid_argument (Fmt.strf "Smart_http.Encoder.pkt_line: \
                                              current PKT line too big (%d >= %d)" has 0xFFFF));

      let x = Fmt.strf "%04x" has in
      Bytes.blit_string x 0 pkt_header 0 4
    in

    ((fun k encoder -> let encoder = schedule_flush write_header encoder in k encoder)
     @@ (fun k encoder -> schedule_bytes k encoder pkt_header)
     @@ (fun k encoder ->
         let rec go = function
           | Continue { continue; encoder; } -> continue encoder |> go
           | Flush _ ->  raise (Invalid_argument "Smart_http.Encoder.pkt_line: flush not expected")
           (* XXX(dinosaure): we not write the PKT header (and the full line) yet. *)
           | End encoder -> k encoder
         in

         kk (fun encoder -> End encoder) encoder |> go)
     @@ flush k)
      encoder

  let w_hash hash k encoder =
    let x = Hash.to_hex hash in
    Minienc.write_string x k encoder

  let w_lf k encoder = Minienc.write_char '\n' k encoder
  let w_sp k encoder = Minienc.write_char ' ' k encoder
  let w_nl k encoder = Minienc.write_char '\000' k encoder

  let w_want hash k encoder =
    let open Minienc in

    (write_string "want "
     @@ w_hash hash k)
      encoder

  let w_want_pkt hash = pkt_line (fun k -> w_want hash @@ w_lf k)

  let noop k encoder = k encoder

  let w_list w_sep w_data l k e =
    let rec aux l k e = match l with
      | [ x ] -> w_data x k e
      | x :: r ->
        (w_data x @@ w_sep @@ aux r k) e
      | [] -> k e
    in
    aux l k e

  let w_capability capability k encoder =
    let s = Capability.to_string capability in
    Minienc.write_string s k encoder

  let w_cap_list l k encoder =
    w_list w_sp w_capability l k encoder

  let w_want_list (first, rest) capabilities k encoder =
    (pkt_line (fun k -> w_want first @@ w_nl @@ w_cap_list capabilities @@ w_lf k)
     @@ w_list noop w_want_pkt rest k)
      encoder

  let w_have_list l k encoder =
    let open Minienc in

    w_list noop (fun hash -> pkt_line (fun k -> write_string "have " @@ w_hash hash @@ w_lf k)) l k encoder

  let w_request_end k encoder =
    Minienc.(write_string "0000" (flush k) encoder)

  let w_done k encoder =
    pkt_line (fun k -> Minienc.write_string "done" @@ w_lf k) k encoder

  let w_compute_request { want; have; capabilities; } k encoder =
    (w_want_list want capabilities
     @@ w_have_list have
     @@ w_request_end
     @@ w_done k) (* TODO: check if [done] terminates the negotiation between the client and the server. *)
      encoder

  let encoder = Minienc.create 0x10000 (* mandatory to be a power of 2! *)

  let encode request x =
    let k encoder = match x with
      | `UploadRequest x -> w_compute_request x (fun encoder -> Minienc.End ()) encoder
    in

    let producer consumer =
      let raw = Cstruct.create 0x800 in
      
      let rec go state =

        match state with
        | Minienc.Flush { continue; iovecs; } ->
          let open Lwt.Infix in

          Lwt_list.fold_left_s (fun acc -> function
              | { Minienc.IOVec.buffer = `Bytes buf; off; len } ->
                Cstruct.blit_from_bytes buf off raw 0 len;
                consumer (Some (Cstruct.to_bigarray raw, 0, len)); Lwt.return (acc + len)
              | { Minienc.IOVec.buffer = `String buf; off; len } ->
                Cstruct.blit_from_string buf off raw 0 len;
                consumer (Some (Cstruct.to_bigarray raw, 0, len)); Lwt.return (acc + len)
              | { Minienc.IOVec.buffer = `Bigstring buf; off; len } ->
                consumer (Some (buf, off, len)); Lwt.return (acc + len))
            0 iovecs
          >|= continue >>= go
        | Minienc.Continue { continue; encoder; } ->
          continue encoder |> go
        | Minienc.End v -> consumer None; Lwt.return v
      in

      k encoder |> go
    in

    let body = Web.Request.stream_body producer in
    Web.Request.with_body request body
end
