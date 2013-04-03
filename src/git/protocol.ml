(*
 * Copyright (c) 2013 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lib

let sp  = '\x20'
let nul = '\x00'
let lf  = '\x0a'

let error fmt =
  Printf.kprintf (fun msg ->
    Printf.printf "Protocol error:%s\n%!" msg;
    raise Parsing.Parse_error
  ) fmt

let debug = ref true

let debug fmt =
  Printf.kprintf (fun str ->
    if !debug then
      Printf.printf "%s\n%!" str
  ) fmt

let of_hex str =
  Model.Node.of_string (Misc.hex_decode str)

let to_hex id =
  Misc.hex_encode (Model.Node.to_string id)

module PacketLine = struct

  type t = string option

  let output oc = function
    | None  ->
      debug "SENDING: FLUSH";
      output_string oc "0000";
      flush oc
    | Some l ->
      debug "SENDING: %S" l;
      let size = Printf.sprintf "%04x" (4 + String.length l) in
      output_string oc size;
      output_string oc l;
      flush oc

  let output_line oc s =
    output oc (Some s)

  let flush oc =
    output oc None

  let input ic =
    let size = String.create 4 in
    really_input ic size 0 4;
    match size with
    | "0000" ->
      debug "RECEIVED: FLUSH";
      None
    | _      ->
      let size = int_of_string ("0x" ^ size) - 4 in
      let payload = String.create size in
      really_input ic payload 0 size;
      debug "RECEIVED: %S" payload;
      if payload.[size - 1] = lf then
        Some (String.sub payload 0 (size-1))
      else
        Some payload

end

module Capability = struct

  type t =
    | Multi_ack
    | Thin_pack
    | Side_band
    | Side_band_64k
    | Ofs_delta
    | Shallow
    | No_progress
    | Include_tag
    | Other of string

  let of_string = function
    | "multi_ack"     -> Multi_ack
    | "thin-pack"     -> Thin_pack
    | "side-band"     -> Side_band
    | "side-band-64k" -> Side_band_64k
    | "ofs-delta"     -> Ofs_delta
    | "shallow"       -> Shallow
    | "no-progress"   -> No_progress
    | "include-tag"   -> Include_tag
    | x               -> Other x

  let to_string = function
    | Multi_ack     -> "multi_ack"
    | Thin_pack     -> "thin-pack"
    | Side_band     -> "side-band"
    | Side_band_64k -> "side-band-64k"
    | Ofs_delta     -> "ofs-delta"
    | Shallow       -> "shallow"
    | No_progress   -> "no-progress"
    | Include_tag   -> "include-tag"
    | Other x       -> x

end

module Capabilities = struct

  type t = Capability.t list

  let of_string str =
    List.map Capability.of_string (Misc.split str sp)

  let to_string l =
    String.concat " " (List.map Capability.to_string l)

  (* XXX really ? *)
  let default = []

end

module Address = struct

  type t = {
    host: string option;
    port: int option;
    path: string;
  }

  let host t = t.host
  let port t = t.port
  let path t = t.path

  let of_string str =
    let uri = Uri.of_string str in
    let host, port = match Uri.host uri with
      | None      -> None, None
      | Some host ->
        let port = Uri.port uri in
        Some host, port in
    let path = Uri.path uri in
    { host; port; path }

end

module Init = struct

  type request =
    | Upload_pack
    | Receive_pack
    | Upload_archive

  let string_of_request = function
    | Upload_pack    -> "git-upload-pack"
    | Receive_pack   -> "git-receive-pack"
    | Upload_archive -> "git-upload-archive"

  type t = {
    request: request;
    address: Address.t;
  }

  let host t = Address.host t.address
  let port t = Address.port t.address

  let output oc t =
    let message =
      let buf = Buffer.create 1024 in
      Buffer.add_string buf (string_of_request t.request);
      Buffer.add_char   buf sp;
      Buffer.add_string buf (Address.path t.address);
      Buffer.add_char   buf nul;
      begin match Address.host t.address with
        | None   -> ()
        | Some h ->
          Buffer.add_string buf "host=";
          Buffer.add_string buf h;
          begin match Address.port t.address with
            | None   -> ()
            | Some p ->
              Buffer.add_char   buf ':';
              Buffer.add_string buf (Printf.sprintf "%d" p);
          end;
          Buffer.add_char buf nul;
      end;
      Buffer.contents buf in
    PacketLine.output_line oc message

  let close oc =
    PacketLine.flush oc

  let create str =
    let address = Address.of_string str in
    { request = Upload_pack; address }

end

module Listing = struct

  type t = {
    capabilities: Capabilities.t;
    references  : (Model.node * string) list;
  }

  let references t = t.references

  let empty = {
    capabilities = [];
    references   = [];
  }

  let head t =
    match List.filter (fun (id,n) -> n="HEAD") t.references with
    | []        -> None
    | (id,_)::_ -> Some id

  let dump t =
    Printf.printf "CAPABILITIES:\n%s\n" (Capabilities.to_string t.capabilities);
    Printf.printf "\nREFERENCES:\n";
    List.iter
      (fun (n,r) -> Printf.printf "%s %s\n%!" (to_hex n) r)
      t.references

  let input ic =
    let rec aux acc =
      match PacketLine.input ic with
      | None      -> { acc with references = List.rev acc.references }
      | Some line ->
        match Misc.cut_at line sp with
        | Some ("ERR", err) -> error "ERROR: %s" err
        | Some (node, ref)  ->
          if acc = empty then (
            (* Read the capabilities on the first line *)
            match Misc.cut_at ref nul with
            | Some (ref, caps) ->
              let pair = (of_hex node, ref) in
              let capabilities = Capabilities.of_string caps in
              aux { references = [ pair ]; capabilities; }
            | None ->
              let pair = (of_hex node, ref) in
              aux { references = [ pair ]; capabilities = []; }
          ) else
            let pair = (of_hex node, ref) in
            aux { acc with references = pair :: acc.references }
        | None -> error "%s is not a valid answer" line
    in
    aux empty

end

module Ack = struct

  type status = Continue
              | Common
              | Ready

  let string_of_status = function
    | "continue" -> Continue
    | "common"   -> Common
    | "ready"    -> Ready
    | x          -> error "%s: invalid ack status" x

  type t =
    | Ack_multi of Model.node * status
    | Ack of Model.node
    | Nak

  let input ic =
    let rec aux acc =
      match PacketLine.input ic with
      | None
      | Some "NAK" -> List.rev (Nak :: acc)
      | Some s      ->
        match Misc.cut_at s sp with
        | Some ("ACK", r) ->
          begin match Misc.cut_at r sp with
            | None         -> aux (Ack (of_hex r) :: acc)
            | Some (id, s) -> aux (Ack_multi (of_hex id, string_of_status s) :: acc)
          end
        | _ -> error "%S invalid ack" s
    in
    aux []

end

module Upload = struct

  type message =
    | Want of Model.node * Capability.t list
    | Shallow of Model.node
    | Deepen of int
    | Unshallow of Model.node
    | Have of Model.node
    | Done

  type t = message list

  let filter fn l =
    List.fold_left (fun acc elt ->
      match fn elt with
      | None   -> acc
      | Some x -> x::acc
    ) [] l

  let wants l =
    filter (function Want (x,y) -> Some (x,y) | _ -> None) l

  let shallows l =
    filter (function Shallow x -> Some x | _ -> None) l

  let deepen l =
    match filter (function Deepen d -> Some d | _ -> None) l with
    | []    -> 0
    |  i::_ -> i

  let unshallows l =
    filter (function Unshallow x -> Some x | _ -> None) l

  let haves l =
    filter (function Have x -> Some x | _ -> None) l

  let create l = l

  let input ic =
    let rec aux acc =
      match PacketLine.input ic with
      | None   -> List.rev acc
      | Some l ->
        match Misc.cut_at l sp with
        | None -> error "input upload"
        | Some (kind, s) ->
          match kind with
          | "shallow"   -> aux (Shallow   (of_hex s) :: acc)
          | "unshallow" -> aux (Unshallow (of_hex s) :: acc)
          | "have"      -> aux (Have      (of_hex s) :: acc)
          | "done"      -> aux (Done                 :: acc)
          | "deepen"    ->
            let d =
              try int_of_string s
              with _ -> error "%s is not a valid integer" s in
            aux (Deepen d :: acc)
          | "want" ->
            let aux id c = aux (Want (of_hex id, c) :: acc) in
            begin match Misc.cut_at s sp with
              | Some (id,c) -> aux id (Capabilities.of_string c)
              | None        -> match acc with
                | Want (_,c)::_ -> aux s c
                | _             -> error "want without capacity"
            end
          | s -> error "%s is not a valid upload request." s
    in
    aux []

  let output oc t =
    let last_c = ref [] in

    (* output wants *)
    List.iter (fun (id, c) ->
      if c = !last_c then
        let msg = Printf.sprintf "want %s\n" (to_hex id) in
        PacketLine.output_line oc msg
      else
        let msg =
          Printf.sprintf "want %s %s\n" (to_hex id) (Capabilities.to_string c) in
        PacketLine.output_line oc msg;
        last_c := c;
    ) (wants t);

    (* output shallows *)
    List.iter (fun id ->
      let msg = Printf.sprintf "shallow %s" (to_hex id) in
      PacketLine.output_line oc msg
    ) (shallows t);

    (* output unshallows *)
    List.iter (fun id ->
      let msg = Printf.sprintf "unshallow %s" (to_hex id) in
      PacketLine.output_line oc msg
    ) (unshallows t);

    (* output haves *)
    List.iter (fun id ->
      let msg = Printf.sprintf "have %s\n" (to_hex id) in
      PacketLine.output_line oc msg
    ) (haves t);

    (* output deepen *)
    let deepen = deepen t in
    if deepen <> 0 then (
      let msg = Printf.sprintf "deepen %d" deepen in
      PacketLine.output_line oc msg;
    );

    (* output done *)
    if List.mem Done t then
      PacketLine.output_line oc "done"
    else
      PacketLine.flush oc

  (* PHASE1: the client send the the IDs he wants, the severs answer
     the new shallow state. *)
  let phase1 (ic, oc) ?deepen ?(shallows=[]) wants =
    let wants = List.map (fun id -> Want (id, Capabilities.default)) wants in
    let shallows = List.map (fun id -> Shallow id) shallows in
    let deepen = match deepen with
      | None   -> []
      | Some d -> [Deepen d] in
    output oc (wants @ shallows @ deepen);
    if deepen <> [] then
      Some (input ic)
    else
      None

  let pick n l =
    let rec aux i acc l =
      if i <= 0 then (List.rev acc, l)
      else match l with
        | []   -> (List.rev acc, l)
        | h::t -> aux (i-1) (h::acc) t
    in
    aux n [] l


  let phase2 (ic, oc) haves =
    let rec aux haves =
      if List.length haves > 32 then
        let head, tail = pick 32 haves in
        output oc head;
        aux tail
      else
        output oc (haves @ [Done])
    in
    let haves = List.map (fun id -> Have id) haves in
    aux haves;
    let _acks = Ack.input ic in
    ()

end

let connect address port =
  let port = match port with
    | None   -> 9418
    | Some p -> p in
  let inet_addr =
    let host = Unix.gethostbyname address in
    host.Unix.h_addr_list.(0) in
  let sockaddr =
    Unix.ADDR_INET (inet_addr, port) in
  Unix.open_connection sockaddr

let todo msg =
  failwith ("TODO: " ^ msg)

let clone t ?(write=false) ?deepen address =
  let r = Init.create address in
  match Init.host r with
  | None   -> todo "no-host"
  | Some h ->
    let ic, oc = connect h (Init.port r) in
    Init.output oc r;
    let listing = Listing.input ic in
    Listing.dump listing;
    List.iter (fun (node, name) ->
      Store.write_reference t name node
    ) (Listing.references listing);
    match Listing.head listing with
    | None    -> Init.close oc
    | Some id ->
      Printf.printf "PHASE1\n%!";
      let _shallows = Upload.phase1 ?deepen (ic,oc) [id] in

      Printf.printf "PHASE2\n%!";
      Upload.phase2 (ic,oc) [];

      Printf.printf "PHASE3\n%!";

      let raw = Misc.string_of_in_channel ic in
      let buf = IO.of_string (File.Name.of_string "<network>") raw in
      let buffers = Hashtbl.create 1024 in
      let read node =
        if Hashtbl.mem buffers node then IO.clone (Hashtbl.find buffers node)
        else error "%s: unknown node" (to_hex node) in
      let write value =
        let inflated = Backend.Value.output_inflated value in
        let node = Model.Node.sha1 inflated in
        if not (Hashtbl.mem buffers node) then (
          let buf = IO.of_string (File.Name.of_string "<init>") inflated in
          Hashtbl.add buffers node buf;
          if write then Store.write_and_check_inflated t node inflated;
        );
        node in

      let nodes = Backend.Pack.unpack_all ~read ~write buf in
      Printf.printf "NEW OBJECTS:\n";
      List.iter (fun n -> Printf.printf "%s\n%!" (to_hex n)) nodes
