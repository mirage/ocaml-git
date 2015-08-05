(*
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt.Infix
open Printf

module Log = Log.Make(struct let section = "sync" end)

type protocol = [ `SSH | `Git | `Smart_HTTP ]

let protocol uri = match Uri.scheme uri with
  | Some "git"     -> `Ok `Git
  | Some "git+ssh" -> `Ok `SSH
  | Some "http"
  | Some "https"   -> `Ok `Smart_HTTP
  | Some x -> `Not_supported x
  | None   -> `Unknown

let err fmt = Printf.ksprintf failwith fmt
let err_unknkown () = err "Unknown Git protocol"
let err_not_supported x = err "%s is not a supported Git protocol" x
let err_unknown_tag t = err "%s: unknown tag" (Reference.pretty t)

let protocol_exn uri = match protocol uri with
  | `Ok x            -> x
  | `Unknown         -> err_unknkown ()
  | `Not_supported x -> err_not_supported x

let pretty_protocol = function
  | `Git -> "git"
  | `SSH -> "ssh"
  | `Smart_HTTP -> "smart-http"

type want = [ `Ref of Reference.t | `Commit of SHA.Commit.t ]

let pretty_list f l = "[" ^ String.concat ", " (List.map f l) ^ "]"

let pretty_want = function
  | `Commit s -> sprintf "commit:%s" (SHA.Commit.pretty s)
  | `Ref r    -> sprintf "ref:%s" (Reference.pretty r)

let pretty_wants = function
  | None   -> "<all>"
  | Some l -> pretty_list pretty_want l

let is_head ref =
  Reference.is_valid ref &&
  let raw_ref = Reference.to_raw ref in
  let prefix = "refs/heads/" in
  match Misc.string_chop_prefix ~prefix raw_ref with
  | None   -> false
  | Some _ -> true

module type IO = sig
  type ic
  type oc
  type ctx
  val with_connection: ?ctx:ctx -> Uri.t -> ?init:string ->
    (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
  val read_all: ic -> string list Lwt.t
  val read_exactly: ic -> int -> string Lwt.t
  val write: oc -> string -> unit Lwt.t
  val flush: oc -> unit Lwt.t
end

let ogit_agent = "git/ogit." ^ Version.current

module Capability = struct

  type t = [
    | `Multi_ack
    | `Thin_pack
    | `Side_band
    | `Side_band_64k
    | `Ofs_delta
    | `Shallow
    | `No_progress
    | `Include_tag
    | `Report_status
    | `Delete_refs
    | `Allow_reachable_sha1_in_want (* in Git 2.5 only *)
    | `Agent of string
    | `Other of string ]

  let of_string: string -> t = function
    | "multi_ack"     -> `Multi_ack
    | "thin-pack"     -> `Thin_pack
    | "side-band"     -> `Side_band
    | "side-band-64k" -> `Side_band_64k
    | "ofs-delta"     -> `Ofs_delta
    | "shallow"       -> `Shallow
    | "no-progress"   -> `No_progress
    | "include-tag"   -> `Include_tag
    | "report-status" -> `Report_status
    | "delete-refs"   -> `Delete_refs
    | "allow-reachable-sha1-in-want" -> `Allow_reachable_sha1_in_want
    | x               ->
      match Stringext.cut x ~on:"=" with
      | Some ("agent", a) -> `Agent a
      | _ -> `Other x

  let to_string: t -> string = function
    | `Multi_ack     -> "multi_ack"
    | `Thin_pack     -> "thin-pack"
    | `Side_band     -> "side-band"
    | `Side_band_64k -> "side-band-64k"
    | `Ofs_delta     -> "ofs-delta"
    | `Shallow       -> "shallow"
    | `No_progress   -> "no-progress"
    | `Include_tag   -> "include-tag"
    | `Report_status -> "report-status"
    | `Delete_refs   -> "delete-refs"
    | `Agent a       -> "agent=" ^ a
    | `Allow_reachable_sha1_in_want -> "allow-reachable-sha1-in-want"
    | `Other x       -> x

  let _is_valid_fetch: t -> bool = function
    | `Multi_ack
    | `Thin_pack
    | `Side_band
    | `Side_band_64k
    | `Ofs_delta
    | `Shallow
    | `No_progress
    | `Include_tag -> true
    | _ -> false

  let _is_valid_push: t -> bool = function
    | `Ofs_delta
    | `Report_status
    | `Delete_refs -> true
    | _ -> false

  let ogit_agent = `Agent ogit_agent

end

type capability = Capability.t

module Capabilities = struct

    type t = Capability.t list

    let of_string str =
      List.map Capability.of_string (Stringext.split str ~on:Misc.sp)

    let to_string l =
      String.concat " " (List.map Capability.to_string l)

    let pretty l =
      String.concat ", " (List.map Capability.to_string l)

    let default = [
      Capability.ogit_agent;
      `Side_band_64k;
    ]

end

module Listing = struct

  type t = {
    capabilities: Capability.t list;
    sha1s       : Reference.t list SHA.Commit.Map.t;
    references  : SHA.Commit.t Reference.Map.t;
  }

  let capabilities t = t.capabilities
  let references t = t.references
  let sha1s t = t.sha1s

  let empty = {
    capabilities = [];
    sha1s        = SHA.Commit.Map.empty;
    references   = Reference.Map.empty;
  }

  let is_empty t = t.capabilities = [] && SHA.Commit.Map.is_empty t.sha1s

  let find_reference t r =
    try Some (Reference.Map.find r t.references)
    with Not_found -> None

  let pretty t =
    let buf = Buffer.create 1024 in
    Printf.bprintf buf "CAPABILITIES:\n%s\n"
      (Capabilities.to_string t.capabilities);
    Printf.bprintf buf "\nREFERENCES:\n";
    SHA.Commit.Map.iter
      (fun key data ->
         List.iter (fun ref ->
             Printf.bprintf buf "%s %s\n"
               (SHA.Commit.to_hex key) (Reference.pretty ref)
           ) data
      ) t.sha1s;
    Buffer.contents buf

end

module Result = struct

  type fetch = { listing: Listing.t; sha1s: SHA.Set.t }

  let head t = Listing.find_reference t.listing Reference.head

  let head_contents t =
    match head t with
    | None   -> None
    | Some c ->
      let heads =
        SHA.Commit.Map.find c t.listing.Listing.sha1s
        |> List.filter is_head
      in
      match heads with
      | []   -> Some (Reference.SHA c)
      | h::_ ->
        let r =
          if List.mem Reference.master heads then Reference.master else (
            if List.length heads > 1 then
              Log.info "Ambiguous remote HEAD, picking %s."
                (Reference.pretty h);
            h
          )
        in
        Some (Reference.Ref r)

  let references t = Listing.references t.listing
  let sha1s t = t.sha1s

  let pretty_head_contents = function
    | None -> "<none>"
    | Some (Reference.Ref r) -> Reference.pretty r
    | Some (Reference.SHA s) -> SHA.Commit.pretty s

  let pretty_head = function
    | None   -> ""
    | Some c -> SHA.Commit.pretty c

  let pretty_fetch t =
    let buf = Buffer.create 1024 in
    let hc = head_contents t in
    let h = head t in
    bprintf buf "HEAD: %s %s\n" (pretty_head_contents hc) (pretty_head h);
    Reference.Map.iter (fun key data ->
        bprintf buf "%s %s\n" (Reference.pretty key) (SHA.Commit.to_hex data)
      ) (references t);
    bprintf buf "Keys: %d\n" (SHA.Set.cardinal t.sha1s);
    Buffer.contents buf

  type ok_or_error = [ `Ok | `Error of string ]

  type push = {
    result: ok_or_error;
    commands: (Reference.t * ok_or_error) list;
  }

  let pretty_push t =
    let buf = Buffer.create 1024 in
    let aux (ref, result) = match result with
      | `Ok      -> Printf.bprintf buf "* %s\n" (Reference.pretty ref)
      | `Error e -> Printf.bprintf buf "! %s: %s\n" (Reference.pretty ref) e
    in
    List.iter aux t.commands;
    Buffer.contents buf

end

module Make (IO: IO) (D: SHA.DIGEST) (I: Inflate.S) (Store: Store.S) = struct

  module SHA_IO = SHA.IO(D)

  exception Error

  type ctx = IO.ctx

  let error fmt =
    Printf.ksprintf (fun msg ->
        Log.error "%s" msg;
        raise Error
      ) fmt

  let err_invalid_integer fn str =
    error "%s: %S is not a valid integer" fn str

  module PacketLine = struct

    type t = string option

    let output oc = function
      | None  ->
        let flush = "0000" in
        Log.info "SENDING: %S" flush;
        IO.write oc flush >>= fun () ->
        IO.flush oc
      | Some l ->
        let size = Printf.sprintf "%04x" (4 + String.length l) in
        Log.info "SENDING: %S" (size ^ l);
        IO.write oc size >>= fun () ->
        IO.write oc l    >>= fun () ->
        IO.flush oc

    let to_string = function
      | None   -> "0000"
      | Some l ->
        let size = Printf.sprintf "%04x" (4 + String.length l) in
        sprintf "%s%s" size l

    let output_line oc s =
      output oc (Some s)

    let string_of_line s =
      to_string (Some s)

    let flush oc =
      output oc None

    let err_no_trailing_lf () =
      error "PacketLine.input: the payload doesn't have a trailing LF"

    let input_raw ic: t Lwt.t =
      Log.debug "PacketLine.input_raw";
      IO.read_exactly ic 4 >>= fun size ->
      match size with
      | "0000" ->
        Log.debug "RECEIVED: FLUSH";
        Lwt.return_none
      | size   ->
        let size =
          let str = "0x" ^ size in
          try int_of_string str - 4
          with Failure _ -> err_invalid_integer "PacketLine.input" str
        in
        IO.read_exactly ic size >>= fun payload ->
        Log.debug "RECEIVED: %S (%d)" payload size;
        Lwt.return (Some payload)

    let niet = Lwt.return (Some "")

    let input ic =
      Log.debug "PacketLine.input";
      input_raw ic >>= function
      | None    -> Lwt.return_none
      | Some "" -> niet
      | Some s  ->
        let size = String.length s in
        if s.[size - 1] <> Misc.lf then err_no_trailing_lf ();
        let s = String.sub s 0 (size-1) in
        Lwt.return (Some s)

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
      discover: bool; (* The smart HTTP protocol has 2 modes. *)
      gri: Gri.t;
    }

    let host t = Uri.host (Gri.to_uri t.gri)
    let uri t = Gri.to_uri t.gri

    (* Initialisation sentence for the Git protocol *)
    let git t =
      let uri = Gri.to_uri t.gri in
      let message =
        let buf = Buffer.create 1024 in
        let path = match Uri.path uri with "" -> "/" | p  -> p in
        Buffer.add_string buf (string_of_request t.request);
        Buffer.add_char   buf Misc.sp;
        Buffer.add_string buf path;
        Buffer.add_char   buf Misc.nul;
        begin match Uri.host uri with
          | None   -> ()
          | Some h ->
            Buffer.add_string buf "host=";
            Buffer.add_string buf h;
            begin match Uri.port uri with
              | None   -> ()
              | Some p ->
                Buffer.add_char   buf ':';
                Buffer.add_string buf (Printf.sprintf "%d" p);
            end;
            Buffer.add_char buf Misc.nul;
        end;
        Buffer.contents buf in
      PacketLine.string_of_line message

    let ssh t =
      sprintf "%s %s" (string_of_request t.request)
        (Uri.path (Gri.to_uri t.gri))

    let smart_http t =
      (* Note: GitHub wants User-Agent to start by `git/`  *)
      let useragent = "User-Agent", ogit_agent in
      let headers : (string * string) list =
        if t.discover
        then [useragent]
        else [
          useragent;
          "Content-Type",
          sprintf "application/x-%s-request" (string_of_request t.request);
        ]
      in
        Marshal.to_string headers []

    let to_string t =
      match protocol_exn (Gri.to_uri t.gri) with
      | `Git -> Some (git t)
      | `SSH -> Some (ssh t)
      | `Smart_HTTP -> Some (smart_http t)

    let create request ~discover gri =
      Log.debug "Init.create request=%s discover=%b gri=%s"
        (string_of_request request) discover (Gri.to_string gri);
      let protocol = protocol_exn (Gri.to_uri gri) in
      let gri = match protocol with
        | `SSH | `Git -> gri
        | `Smart_HTTP ->
          let service = if discover then "info/refs?service=" else "" in
          let url = Gri.to_string gri in
          let request = string_of_request request in
          Gri.of_string (sprintf "%s/%s%s" url service request)
      in
      Log.debug "computed-gri: %s" (Gri.to_string gri);
      { request; discover; gri }

    let upload_pack = create Upload_pack
    let receive_pack = create Receive_pack
    let _upload_archive = create Upload_archive
  end


  module Listing = struct

    include Listing

    let input ic protocol =
      Log.debug "Listing.input (protocol=%s)" (pretty_protocol protocol);
      let error fmt = error ("[SMART-HTTP] Listing.input:" ^^ fmt) in
      let skip_smart_http () =
        match protocol with
        | `Git | `SSH -> Lwt.return_unit
        | `Smart_HTTP ->
          PacketLine.input ic >>= function
          | None      -> error "missing # header."
          | Some line ->
            match Stringext.cut line ~on:Misc.sp_str with
            | Some ("#", service) ->
              Log.debug "skipping %s" service;
              begin PacketLine.input ic >>= function
              | None   -> Lwt.return_unit
              | Some x -> error "waiting for pkt-flush, got %S" x
              end
            | Some _ -> error "waiting for # header, got %S" line
            | None   -> error "waiting for # header, got pkt-flush"
      in
      let rec aux acc =
        PacketLine.input ic >>= function
        | None      -> Lwt.return acc
        | Some line ->
          match Stringext.cut line ~on:Misc.sp_str with
          | Some ("ERR", err) -> error "ERROR: %s" err
          | Some (sha1, r)  ->
            let sha1 = SHA_IO.Commit.of_hex sha1 in
            if is_empty acc then (
              (* Read the capabilities on the first line *)
              match Stringext.cut r ~on:Misc.nul_str with
              | Some (r, caps) ->
                let r = Reference.of_raw r in
                let sha1s = SHA.Commit.Map.add_multi sha1 r acc.sha1s in
                let references = Reference.Map.add r sha1 acc.references in
                let capabilities = Capabilities.of_string caps in
                aux { sha1s; capabilities; references }
              | None ->
                let r = Reference.of_raw r in
                let sha1s = SHA.Commit.Map.add_multi sha1 r acc.sha1s in
                let references = Reference.Map.add r sha1 acc.references in
                aux { sha1s; references; capabilities = [] }
            ) else
              let r = Reference.of_raw r in
              let sha1s = SHA.Commit.Map.add_multi sha1 r acc.sha1s in
              let references = Reference.Map.add r sha1 acc.references in
              aux { acc with sha1s; references }
          | None -> error "Listing.input: %S is not a valid answer" line
      in
      skip_smart_http () >>= fun () ->
      aux empty

  end

  module Ack = struct

    type status = Continue
                | Common
                | Ready

    let status_of_string = function
      | "continue" -> Continue
      | "common"   -> Common
      | "ready"    -> Ready
      | x          -> error "%s: invalid ack status" x

    type t =
      | Ack_multi of SHA.t * status
      | Ack of SHA.t
      | Nak

    let input ic =
      Log.debug "Ack.input";
      PacketLine.input ic >>= function
      | None
      | Some "NAK" -> Lwt.return Nak
      | Some s      ->
        match Stringext.cut s ~on:Misc.sp_str with
        | Some ("ACK", r) ->
          begin match Stringext.cut r ~on:Misc.sp_str with
            | None         -> Lwt.return (Ack (SHA_IO.of_hex r))
            | Some (id, s) ->
              Lwt.return (Ack_multi (SHA_IO.of_hex id, status_of_string s))
          end
        | _ -> error "%S invalid ack" s

    let _inputs ic =
      Log.debug "Ack.inputs";
      let rec aux acc =
        input ic >>= function
        | Nak -> Lwt.return (List.rev (Nak :: acc))
        | tok -> aux (tok :: acc)
      in
      aux []

  end

  module Upload_request = struct

    type message =
      | Want of SHA.Commit.t * Capability.t list
      | Shallow of SHA.t
      | Deepen of int
      | Unshallow of SHA.t
      | Have of SHA.t
      | Done

    type t = message list

    let filter fn l =
      List.fold_left (fun acc elt ->
          match fn elt with
          | None   -> acc
          | Some x -> x::acc
        ) [] l
      |> List.rev

    let filter_wants l =
      filter (function Want (x,y) -> Some (x,y) | _ -> None) l

    let filter_shallows l =
      filter (function Shallow x -> Some x | _ -> None) l

    let filter_deepen l =
      match filter (function Deepen d -> Some d | _ -> None) l with
      | []    -> 0
      |  i::_ -> i

    let filter_unshallows l =
      filter (function Unshallow x -> Some x | _ -> None) l

    let filter_haves l =
      filter (function Have x -> Some x | _ -> None) l

    let input ic: t Lwt.t =
      Log.debug "Upload.input";
      let rec aux acc =
        PacketLine.input_raw ic >>= function
        | None   -> Lwt.return (List.rev acc)
        | Some l ->
          match Stringext.cut l ~on:Misc.sp_str with
          | None -> error "input upload"
          | Some (kind, s) ->
            match kind with
            | "shallow"   -> aux (Shallow   (SHA_IO.of_hex s) :: acc)
            | "unshallow" -> aux (Unshallow (SHA_IO.of_hex s) :: acc)
            | "have"      -> aux (Have      (SHA_IO.of_hex s) :: acc)
            | "done"      -> aux (Done                      :: acc)
            | "deepen"    ->
              let d =
                try int_of_string s
                with Failure _ -> err_invalid_integer "Upload.input" s
              in
              aux (Deepen d :: acc)
            | "want" ->
              let aux id c = aux (Want (SHA_IO.Commit.of_hex id, c) :: acc) in
              begin match Stringext.cut s ~on:Misc.sp_str with
                | Some (id,c) -> aux id (Capabilities.of_string c)
                | None        -> match acc with
                  | Want (_,c)::_ -> aux s c
                  | _             -> error "want without capacity"
              end
            | s -> error "Upload.input: %S is not a valid upload request." s
      in
      aux []

    (* XXX: handle multi_hack *)
    let output oc t =
      Log.debug "Upload.output";

      (* output wants *)
      Lwt_list.iteri_s (fun i (id, c) ->
          if i = 0 && c <> [] then
            (* first-want *)
            let msg = Printf.sprintf
                "want %s %s\n" (SHA.Commit.to_hex id) (Capabilities.to_string c)
            in
            PacketLine.output_line oc msg
          else
            (* additional-want *)
            let msg = Printf.sprintf "want %s\n" (SHA.Commit.to_hex id) in
            if i <> 0 && c <> [] then
              Log.warn "additional-want: ignoring %s."
                (Capabilities.to_string c);
            PacketLine.output_line oc msg
        ) (filter_wants t)
      >>= fun () ->

      (* output shallows *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "shallow %s" (SHA.to_hex id) in
          PacketLine.output_line oc msg
        ) (filter_shallows t)
      >>= fun () ->

      (* output unshallows *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "unshallow %s" (SHA.to_hex id) in
          PacketLine.output_line oc msg
        ) (filter_unshallows t)
      >>= fun () ->

      (* output haves *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "have %s\n" (SHA.to_hex id) in
          PacketLine.output_line oc msg >>= fun () ->
          Lwt.return_unit
        ) (filter_haves t)
      >>= fun () ->

      (* output deepen *)
      let deepen = filter_deepen t in
      begin if deepen <> 0 then (
          let msg = Printf.sprintf "deepen %d" deepen in
          PacketLine.output_line oc msg;
        ) else
          Lwt.return_unit
      end >>= fun () ->

      (* output done *)
      if List.mem Done t then
        PacketLine.output_line oc "done\n"
      else
        PacketLine.flush oc

    type phase1_result = {
      shallows: SHA.t list;
      unshallows: SHA.t list;
    }

    (* PHASE1: the client send the the IDs he wants, the sever answers with
       the new shallow state. *)
    let phase1 (ic, oc) ?deepen ~capabilities ~shallows ~wants =
      Log.debug "Upload.phase1";
      let wants =
        let want id = Want (id, []) in
        match wants with
        | []   -> []
        | h::t -> Want (h, capabilities) :: List.map want t
      in
      let shallows = List.map (fun id -> Shallow id) shallows in
      let deepen = match deepen with
        | None   -> []
        | Some d -> [Deepen d] in
      output oc (wants @ shallows @ deepen) >>= fun () ->
      if deepen <> [] then
        input ic >>= fun res ->
        let shallows = filter_shallows res in
        let unshallows = filter_unshallows res in
        Lwt.return (Some { shallows; unshallows })
      else
        Lwt.return_none

    let pick n l =
      let rec aux i acc l =
        if i <= 0 then (List.rev acc, l)
        else match l with
          | []   -> (List.rev acc, l)
          | h::t -> aux (i-1) (h::acc) t
      in
      aux n [] l

    let phase2 (ic, oc) ~haves =
      let rec aux haves =
        if List.length haves > 32 then
          let head, tail = pick 32 haves in
          output oc head >>= fun () ->
          (* XXX: the client can notify the servers that it has received
             enough ACK (?) and is ready to receive the pack file by
             sending an early 'done' *)
          (* XXX: the client is supposed to give-up after sending 256 keys
             without receiving any 'ACKS <key> continue'. *)
          aux tail
        else
          output oc (haves @ [Done])
      in
      let haves = List.map (fun id -> Have id) haves in
      aux haves >>= fun () ->
      Ack.input ic >>= fun _ack ->
      Lwt.return_unit

  end

  module Side_band = struct

    (* This capability means that server can send, and client
       understand multiplexed progress reports and error info
       interleaved with the packfile itself.

       These two options are mutually exclusive. A modern client
       always favors 'side-band-64k'.

       Either mode indicates that the packfile data will be streamed
       broken up into packets of up to either 1000 bytes in the case
       of 'side_band', or 65520 bytes in the case of
       'side_band_64k'. Each packet is made up of a leading 4-byte
       pkt-line length of how much data is in the packet, followed by
       a 1-byte stream code, followed by the actual data.

       The stream code can be one of:

       1 - pack data
       2 - progress messages
       3 - fatal error message just before stream aborts

       The "side-band-64k" capability came about as a way for newer
       clients that can handle much larger packets to request packets
       that are actually crammed nearly full, while maintaining
       backward compatibility for the older clients.

       Further, with side-band and its up to 1000-byte messages, it's
       actually 999 bytes of payload and 1 byte for the stream
       code. With side-band-64k, same deal, you have up to 65519 bytes
       of data and 1 byte for the stream code.

       The client MUST send only maximum of one of "side-band" and "side-
       band-64k".  Server MUST diagnose it as an error if client requests
       both.  *)

    type kind = Pack | Progress | Fatal

    let kind c = match Char.code c with
      | 1 -> Pack
      | 2 -> Progress
      | 3 -> Fatal
      | i -> error "Side_band: %d is not a valid message type" i

    exception Error of string

    let input ?(progress=fun _ -> ()) ic =
      Log.debug "Side_band.input";
      let size = ref 0 in
      let t0 = Sys.time () in
      let pp s =
        size := !size + String.length s;
        let mib = 1024. *. 1024. in
        let total = float_of_int !size /. mib in
        let per_s = total /. (Sys.time () -. t0) in
        let done_ = if s = "" then ", done.\n" else "" in
        let str =
          sprintf "Receiving objects: %.2f MiB | %.2f MiB/s%s" total per_s done_
        in
        progress str
      in
      let rec aux acc =
        PacketLine.input_raw ic >>= function
        | None    -> pp ""; Lwt.return (List.rev acc)
        | Some "" -> aux acc
        | Some s  ->
          let payload = String.sub s 1 (String.length s - 1) in
          pp payload;
          match kind s.[0] with
          | Pack     -> aux (payload :: acc)
          | Fatal    -> Lwt.fail (Error payload)
          | Progress ->
            let payload = "remote: " ^ payload in
            Log.info "%s" payload;
            progress payload;
            aux acc
      in
      aux []

  end

  module Pack_file = struct

    let input ~capabilities ?progress ic =
      if List.mem `Side_band_64k capabilities
      || List.mem `Side_band capabilities
      then Side_band.input ?progress ic
      else IO.read_all ic

  end

  module Update_request = struct

    module Pack_IO = Pack.IO(D)(I)

    type command =
      | Create of Reference.t * SHA.Commit.t
      | Delete of Reference.t * SHA.Commit.t
      | Update of Reference.t * SHA.Commit.t * SHA.Commit.t

    let pretty_command t =
      let r = Reference.pretty in
      let c = SHA.Commit.to_hex in
      match t with
      | Create (name, new_id)         ->
        sprintf "create %s %s" (r name) (c new_id)
      | Delete (name, old_id)         ->
        sprintf "delete %s %s" (r name) (c old_id)
      | Update (name, old_id, new_id) ->
        sprintf "update %s %s %s" (r name) (c old_id) (c new_id)

    let pretty_commands l =
      String.concat " & " (List.map pretty_command l)

    let output_command buf t =
      let zero = SHA_IO.Commit.zero in
      let old_id, new_id, name = match t with
        | Create (name, new_id) -> zero, new_id, name
        | Delete (name, old_id) -> old_id, zero, name
        | Update (name, old_id, new_id) -> old_id, new_id, name in
      Printf.bprintf buf "%s %s %s"
        (SHA.Commit.to_hex old_id)
        (SHA.Commit.to_hex new_id)
        (Reference.to_raw name)

    type t = {
      capabilities: Capabilities.t;
      commands: command list;
      pack: Pack.t;
    }

    let pretty t =
      sprintf "UPDATE_REQUEST:\n%s\n%s\npack: %d"
        (Capabilities.pretty t.capabilities)
        (pretty_commands t.commands)
        (List.length t.pack)

    let output oc t =
      let rec aux first = function
        | []   -> PacketLine.flush oc
        | x::y ->
          let buf = Buffer.create 1024 in
          output_command buf x;
          if first then (
            Buffer.add_char buf Misc.nul;
            Buffer.add_string buf (Capabilities.to_string t.capabilities);
          );
          PacketLine.output_line oc (Buffer.contents buf) >>= fun () ->
          aux false y in
      aux true t.commands >>= fun () ->
      let _, buf = Pack_IO.add t.pack in
      let buf = Mstruct.of_cstruct buf in
      let rec send () =
        match Mstruct.length buf with
        | 0 -> Lwt.return_unit
        | n ->
          let len = min 4096 n in
          Log.info "SENDING: %d bytes" len;
          let buf = Mstruct.get_string buf len in
          IO.write oc buf >>=
          send
      in
      send ()

  end

  module Report_status = struct

    let input ic =
      PacketLine.input ic >>= function
      | None -> Lwt.fail (Failure "Report_status.input: empty")
      | Some line ->
        begin match Stringext.cut line ~on:Misc.sp_str with
          | Some ("unpack", "ok") -> Lwt.return `Ok
          | Some ("unpack", err ) -> Lwt.return (`Error err)
          | _ -> Lwt.fail (Failure "Report_status.input: unpack-status")
        end >>= fun result ->
        let aux acc =
          PacketLine.input ic >>= function
          | None      -> Lwt.return acc
          | Some line ->
            match Stringext.cut line ~on:Misc.sp_str with
            | Some ("ok", name)  ->
              Lwt.return ((Reference.of_raw name, `Ok) :: acc)
            | Some ("ng", cont)  ->
              begin match Stringext.cut cont ~on:Misc.sp_str with
                | None  -> Lwt.fail (Failure "Report_status.input: command-fail")
                | Some (name, err) ->
                  Lwt.return ((Reference.of_raw name, `Error err) :: acc)
              end
            | _ -> Lwt.fail (Failure "Report_status.input: command-status")
        in
        aux [] >>= fun commands ->
        Lwt.return { Result.result; commands }

  end

  let todo msg =
    Lwt.fail (Failure ("TODO: " ^ msg))

  type fetch = {
    haves       : SHA.t list;
    shallows    : SHA.t list;
    deepen      : int option;
    unpack      : bool;
    capabilities: Capabilities.t;
    wants       : want list option;
    update      : bool;
  }

  type op =
    | Ls
    | Fetch of fetch

  module Graph = Global_graph.Make(Store)
  module Pack_IO = Pack.IO(D)(I)

  let push ?ctx t ~branch gri =
    Log.debug "Sync.push";
    let init = Init.receive_pack ~discover:true gri in
    match Init.host init with
    | None   -> todo "local-clone"
    | Some _ ->
      let uri = Init.uri init in
      let protocol = protocol_exn uri in
      let init = Init.to_string init in
      IO.with_connection ?ctx uri ?init (fun (ic, oc) ->
          Listing.input ic protocol >>= fun listing ->
          (* XXX: check listing.capabilities *)
          Log.debug "listing:\n %s" (Listing.pretty listing);
          Store.read_reference t branch    >>= fun new_obj ->
          let old_obj = Listing.find_reference listing branch in
          let command = match old_obj, new_obj with
            | None  , None   -> err_unknown_tag branch
            | Some x, None   -> Update_request.Delete (branch, x)
            | None  , Some x -> Update_request.Create (branch, x)
            | Some x, Some y -> Update_request.Update (branch, x, y) in
          let capabilities =
            `Report_status :: match command with
            | Update_request.Delete _ -> [`Delete_refs]
            | _                       -> [`Ofs_delta ]
          in
          let commands = [ command ] in
          let min =
            SHA.Commit.Map.keys (Listing.sha1s listing)
            |> List.map SHA.of_commit
            |> SHA.Set.of_list
          in
          let max = match new_obj with
            | None   -> SHA.Set.empty
            | Some x -> SHA.Set.singleton (SHA.of_commit x)
          in
          Graph.pack t ~min max >>= fun values ->
          let pack = Pack_IO.create values in
          let request = { Update_request.capabilities; commands; pack } in
          Log.debug "request:\n%s" (Update_request.pretty request);
          Update_request.output oc request >>= fun () ->
          Report_status.input ic
        )

  let fetch_commits t (ic, oc) ?(progress=fun _ -> ()) f listing wants =
    Log.debug "Sync.fetch_commits %s" (pretty_list SHA.Commit.pretty wants);
    let wants =
      let w = SHA.Commit.Set.of_list wants in
      let h = SHA.Commit.Set.of_list (List.map SHA.to_commit f.haves) in
      SHA.Commit.Set.diff w h
      |> SHA.Commit.Set.to_list
    in
    if wants = [] then (
      Log.debug "Nothing to want: nothing to do! skip the pack file read.";
      progress "Already up-to-date.\n";
      Lwt.return { Result.listing; sha1s = SHA.Set.empty }
    ) else (
      Log.debug "PHASE1";
      let deepen = f.deepen in
      let capabilities = f.capabilities in
      let shallows = f.shallows in
      Upload_request.phase1 (ic, oc) ?deepen ~capabilities
        ~shallows ~wants
      >>= fun _phase1 ->

      (* XXX: process the shallow / unshallow.  *)
      (* XXX: need a notion of shallow/unshallow in API. *)

      Log.debug "PHASE2";
      let haves = f.haves in
      Upload_request.phase2 (ic,oc) ~haves >>= fun () ->

      Log.debug "PHASE3";
      progress "Receiving data ...\n";
      Pack_file.input ~capabilities ~progress ic >>= fun bufs ->

      let size = List.fold_left (fun acc s -> acc + String.length s) 0 bufs in
      Log.info "Received a pack file of %d bytes." size;
      let pack = Cstruct.create size in
      let _size = List.fold_left (fun acc buf ->
          let len = String.length buf in
          Cstruct.blit_from_string buf 0 pack acc len;
          acc + len
        ) 0 bufs in

      Log.debug "unpack=%b" f.unpack;
      let read = Store.read_inflated t in
      Pack_IO.Raw.input ~progress ~read (Mstruct.of_cstruct pack) >>= fun pack ->
      let unpack () =
        if f.unpack
        then Pack_IO.Raw.unpack ~progress ~write:(Store.write_inflated t) pack
        else Store.write_pack t pack
      in
      unpack () >>= fun sha1s ->
      match SHA.Set.cardinal sha1s with
      | 0 ->
        Log.debug "No new objects";
        progress "Already up-to-date.\n";
        Lwt.return { Result.listing; sha1s }
      | n ->
        Log.debug "%d new objects" n;
        Lwt.return { Result.listing; sha1s }
    )

  let write_heads t reference sha1 =
    if is_head reference then
      Store.mem t (SHA.of_commit sha1) >>= function
      | false -> Lwt.return_unit
      | true  -> Store.write_reference t reference sha1
    else
      Lwt.return_unit

  (* Query the remote store for its references and its HEAD. *)
  let with_listing ?ctx gri k =
    Log.debug "Sync.with_listing";
    let init = Init.upload_pack ~discover:true gri in
    match Init.host init with
    | None   -> todo "local-clone"
    | Some _ ->
      let uri = Init.uri init in
      let protocol = protocol_exn uri in
      let init = Init.to_string init in
      IO.with_connection ?ctx uri ?init (fun (ic, oc) ->
          Listing.input ic protocol >>= fun listing ->
          Log.debug "listing:\n %s" (Listing.pretty listing);
          k (protocol, ic, oc) listing
        )

  let err_sha1_not_advertised sha1 =
    err
      "Cannot fetch %s as the server does not advertise \
       'allow-reachable-sha1-in-want' and it is not in the \
       list of head commits advertised by `upload-pack`."
      (SHA.Commit.pretty sha1)

  let fetch_pack ?ctx ?progress t gri op =
    with_listing ?ctx gri (fun (protocol, ic, oc) listing ->
        match op with
        | Ls      -> Lwt.return { Result.listing; sha1s = SHA.Set.empty }
        | Fetch f ->
          let references = Listing.references listing in
          let commits = match f.wants with
            | None   ->
              (* We ask for all the remote references *)
              Reference.Map.fold (fun r c acc ->
                  if is_head r then SHA.Commit.Set.add c acc else acc
                ) references SHA.Commit.Set.empty
              |> SHA.Commit.Set.elements
            | Some wants ->
              let allow_sha1 =
                let caps = Listing.capabilities listing in
                let all = List.mem `Allow_reachable_sha1_in_want caps in
                fun sha1 ->
                  all ||
                  let sha1s = Listing.sha1s listing in
                  try List.exists is_head (SHA.Commit.Map.find sha1 sha1s)
                  with Not_found -> false
              in
              List.fold_left (fun acc -> function
                  | `Commit c ->
                    if allow_sha1 c then SHA.Commit.Set.add c acc
                    else err_sha1_not_advertised c
                  | `Ref r    ->
                    try
                      let c = Reference.Map.find r references in
                      SHA.Commit.Set.add c acc
                    with Not_found ->
                      acc
                ) SHA.Commit.Set.empty wants
              |> SHA.Commit.Set.elements
          in
          let sync () =
            if protocol = `Smart_HTTP then
              let init = Init.upload_pack ~discover:false gri in
              let uri = Init.uri init in
              let init = Init.to_string init in
              IO.with_connection ?ctx uri ?init (fun (ic, oc) ->
                  fetch_commits t (ic, oc) ?progress f listing commits
                )
            else
              fetch_commits t (ic, oc) ?progress f listing commits
          in
          let update_refs () =
            if not f.update then Lwt.return_unit
            else match f.wants with
              | None       -> Lwt.return_unit
              | Some wants ->
                Lwt_list.iter_p (function
                    | `Commit _ -> Lwt.return_unit
                    | `Ref r    ->
                      try
                        let c = Reference.Map.find r references in
                        write_heads t r c
                      with Not_found ->
                        Lwt.return_unit
                  ) wants
          in
          sync ()        >>= fun r ->
          update_refs () >|= fun () ->
          r
      )

  let ls ?ctx t gri =
    Log.debug "ls %s" (Gri.to_string gri);
    fetch_pack ?ctx t gri Ls >|= fun r ->
    Result.references r

  let fetch
      ?ctx ?deepen ?(unpack=false) ?(capabilities=Capabilities.default)
      ?wants ?(update=false) ?progress
      t gri =
    Log.debug "fetch %s wants=%s" (Gri.to_string gri) (pretty_wants wants);
    Store.list t >>= fun haves ->
    (* XXX: Store.shallows t >>= fun shallows *)
    let shallows = [] in
    let op = { shallows; haves; deepen; unpack; capabilities; update; wants } in
    fetch_pack ?ctx ?progress t gri (Fetch op)

  let populate ?head ?(progress=fun _ -> ()) t ~checkout result =
    let update_head () =
      match head with
      | Some b -> Store.write_head t b
      | None   ->
        match Result.head_contents result with
        | None   -> Lwt.return_unit
        | Some h -> Store.write_head t h
    in
    let update_checkout () =
      if not checkout then Lwt.return_unit
      else match Result.head result with
        | None      -> Lwt.return_unit
        | Some head ->
          Store.write_index t head >>= fun () ->
          progress (sprintf "HEAD is now at %s\n" (SHA.Commit.to_hex head));
          Lwt.return_unit
    in
    update_head () >>= update_checkout

  type t = Store.t

end

module type S = sig
  type t
  type ctx
  val ls: ?ctx:ctx -> t -> Gri.t -> SHA.Commit.t Reference.Map.t Lwt.t
  val push: ?ctx:ctx -> t -> branch:Reference.t -> Gri.t -> Result.push Lwt.t
  val fetch:
    ?ctx:ctx ->
    ?deepen:int ->
    ?unpack:bool ->
    ?capabilities:capability list ->
    ?wants:want list ->
    ?update:bool ->
    ?progress:(string -> unit) ->
    t -> Gri.t -> Result.fetch Lwt.t
  val populate:
    ?head:Reference.head_contents ->
    ?progress:(string -> unit) ->
    t -> checkout:bool -> Result.fetch -> unit Lwt.t
end
