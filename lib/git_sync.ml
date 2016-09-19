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

open Astring
open Lwt.Infix

module Log = (val Git_misc.src_log "sync" : Logs.LOG)

type protocol = [ `SSH | `Git | `Smart_HTTP ]

let protocol uri = match Uri.scheme uri with
  | Some "git"     -> `Ok `Git
  | Some "git+ssh" -> `Ok `SSH
  | Some "http"
  | Some "https"   -> `Ok `Smart_HTTP
  | Some x -> `Not_supported x
  | None   -> `Unknown

let err fmt = Fmt.kstrf failwith fmt
let err_unknkown () = err "Unknown Git protocol"
let err_not_supported x = err "%s is not a supported Git protocol" x
let err_unknown_tag t = err "%a: unknown tag" Git_reference.pp t

let protocol_exn uri = match protocol uri with
  | `Ok x            -> x
  | `Unknown         -> err_unknkown ()
  | `Not_supported x -> err_not_supported x

let pp_protocol ppf = function
  | `Git -> Fmt.string ppf "git"
  | `SSH -> Fmt.string ppf "ssh"
  | `Smart_HTTP -> Fmt.string ppf "smart-http"

type want = [ `Ref of Git_reference.t | `Commit of Git_hash.Commit.t ]

let pp_want ppf = function
  | `Commit s -> Fmt.pf ppf "commit:%a" Git_hash.Commit.pp s
  | `Ref r    -> Fmt.pf ppf "ref:%a" Git_reference.pp r

let pp_wants ppf = function
  | None   -> Fmt.string ppf "<all>"
  | Some l -> Fmt.pf ppf "%a" (Fmt.list pp_want) l

let has_prefix prefix r =
  Git_reference.is_valid r &&
  String.is_prefix ~affix:prefix (Git_reference.to_raw r)

let is_head = has_prefix "refs/heads/"
let is_tag  = has_prefix "refs/tags/"
let is_head_or_tag r = is_head r || is_tag r

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

let ogit_agent = "git/ogit.%%VERSION%%"

module Capability = struct

  type t = [
    | `Multi_ack
    | `Thin_pack
    | `No_thin
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
    | "no-thin"       -> `No_thin
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
      match String.cut x ~sep:"=" with
      | Some ("agent", a) -> `Agent a
      | _ -> `Other x

  let to_string: t -> string = function
    | `Multi_ack     -> "multi_ack"
    | `Thin_pack     -> "thin-pack"
    | `No_thin       -> "no-thin"
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

  let pp = Fmt.of_to_string to_string

end

type capability = Capability.t
let pp_capability = Capability.pp

module Capabilities = struct

  type t = Capability.t list

  let of_string str =
    List.map Capability.of_string (String.cuts str ~sep:Git_misc.sp_str)

  let to_string l =
    String.concat ~sep:" " (List.map Capability.to_string l)

  let pp l = Fmt.(list ~sep:(const string ", ") Capability.pp) l

  let default = [
    Capability.ogit_agent;
    `Side_band_64k;
    `Ofs_delta;
    `Thin_pack;
  ]

  let restrict x y =
    List.filter (function
        | `Agent _   -> true
        | `Thin_pack ->
          (* Receive-pack [..] can ask the client not to use the
             feature by advertising the 'no-thin' capability. A
             client MUST NOT send a thin pack if the server
             advertises the 'no-thin' capability.  *)
          not (List.mem `No_thin y)
        | x -> List.mem x y
      ) x

end

module Listing = struct

  type t = {
    capabilities: Capability.t list;
    hashes      : Git_reference.t list Git_hash.Map.t;
    references  : Git_hash.t Git_reference.Map.t;
  }

  let capabilities t = t.capabilities
  let references t = t.references
  let hashes t = t.hashes

  let empty = {
    capabilities = [];
    hashes       = Git_hash.Map.empty;
    references   = Git_reference.Map.empty;
  }

  let is_empty t = t.capabilities = [] && Git_hash.Map.is_empty t.hashes

  let find_reference t r =
    try Some (Git_reference.Map.find r t.references)
    with Not_found -> None

  let find_hash t c =
    try Git_hash.Map.find c t.hashes
    with Not_found -> []

  let pp ppf t =
    Fmt.pf ppf "CAPABILITIES:\n%a\n" Capabilities.pp t.capabilities;
    Fmt.pf ppf "\nREFERENCES:\n";
    Git_hash.Map.iter
      (fun key data ->
         List.iter (fun r ->
             Fmt.pf ppf "%a %a\n" Git_hash.pp key Git_reference.pp r
           ) data
      ) t.hashes

  let guess_reference t c =
    let heads = Git_hash.Map.find c t.hashes |> List.filter is_head in
    match heads with
    | []   -> None
    | h::_ ->
      let r =
        if List.mem Git_reference.master heads then Git_reference.master else (
          if List.length heads > 1 then
            Log.info (fun l ->
                l "Ambiguous remote HEAD, picking %a." Git_reference.pp h);
          h
        )
      in
      Some r

end

module Result = struct

  type fetch = { listing: Listing.t; hashes: Git_hash.Set.t }

  let head t =
    match Listing.find_reference t.listing Git_reference.head with
    | None   -> None
    | Some h -> Some (Git_hash.to_commit h)

  let head_contents t =
    match head t with
    | None   -> None
    | Some c ->
      match Listing.guess_reference t.listing (Git_hash.of_commit c) with
      | None   -> Some (Git_reference.Hash c)
      | Some r -> Some (Git_reference.Ref r)

  let references t = Listing.references t.listing
  let hashes t = t.hashes

  let pp_head_contents ppf = function
    | None                    -> Fmt.string ppf "<none>"
    | Some (Git_reference.Ref r)  -> Git_reference.pp ppf r
    | Some (Git_reference.Hash s) -> Git_hash.Commit.pp ppf s

  let pp_head ppf = function
    | None   -> Fmt.string ppf ""
    | Some c -> Git_hash.Commit.pp ppf c

  let pp_fetch ppf t =
    let hc = head_contents t in
    let h = head t in
    Fmt.pf ppf "HEAD: %a %a\n" pp_head_contents hc pp_head h;
    Git_reference.Map.iter (fun key data ->
        Fmt.pf ppf "%a %a\n" Git_reference.pp key Git_hash.pp data
      ) (references t);
    Fmt.pf ppf "Keys: %d\n" (Git_hash.Set.cardinal t.hashes)

  type ok_or_error = [ `Ok | `Error of string ]

  type push = {
    result: ok_or_error;
    commands: (Git_reference.t * ok_or_error) list;
  }

  let pp_push ppf t =
    let aux (r, result) = match result with
      | `Ok      -> Fmt.pf ppf "* %a\n" Git_reference.pp r
      | `Error e -> Fmt.pf ppf "! %a: %s\n" Git_reference.pp r e
    in
    List.iter aux t.commands

end

module Make (IO: IO) (Store: Git_store.S) = struct

  module Hash_IO = Git_hash.IO(Store.Digest)

  exception Error

  type ctx = IO.ctx

  let error fmt =
    Printf.ksprintf (fun msg ->
        Log.err (fun l -> l "%s" msg);
        raise Error
      ) fmt

  let err_invalid_integer fn str =
    error "%s: %S is not a valid integer" fn str

  let err_end_of_file () =
    err "The connection has been closed by the server. This is usually due \
         to an invalid client request."

  module PacketLine = struct

    type t = string option

    let output oc = function
      | None  ->
        let flush = "0000" in
        Log.info (fun l -> l "SENDING: %S" flush);
        IO.write oc flush >>= fun () ->
        IO.flush oc
      | Some l ->
        let size = Printf.sprintf "%04x" (4 + String.length l) in
        Log.info (fun log -> log "SENDING: %S" (size ^ l));
        IO.write oc size >>= fun () ->
        IO.write oc l    >>= fun () ->
        IO.flush oc

    let to_string = function
      | None   -> "0000"
      | Some l ->
        let size = Printf.sprintf "%04x" (4 + String.length l) in
        Printf.sprintf "%s%s" size l

    let output_line oc s =
      output oc (Some s)

    let string_of_line s =
      to_string (Some s)

    let flush oc =
      output oc None

    let err_no_trailing_lf () =
      error "PacketLine.input: the payload doesn't have a trailing LF"

    let truncate s =
      if String.length s > 100 then
        String.Ascii.escape (String.with_range ~len:96 s) ^ "[..]"
      else
        String.Ascii.escape s

    let input_raw_exn ic: t Lwt.t =
      Log.debug (fun l -> l "PacketLine.input_raw");
      IO.read_exactly ic 4 >>= fun size ->
      match size with
      | "0000" ->
        Log.debug (fun l -> l "RECEIVED: FLUSH");
        Lwt.return_none
      | size   ->
        let size =
          let str = "0x" ^ size in
          try int_of_string str - 4
          with Failure _ -> err_invalid_integer "PacketLine.input" str
        in
        IO.read_exactly ic size >>= fun payload ->
        Log.debug (fun l -> l "RECEIVED: %s (%d)"  (truncate payload) size);
        Lwt.return (Some payload)

    let input_raw ic =
      Lwt.catch
        (fun () -> input_raw_exn ic)
        (function
          | End_of_file -> err_end_of_file ()
          | e -> Lwt.fail e)

    let niet = Lwt.return (Some "")

    let input ic =
      Log.debug (fun l -> l "PacketLine.input");
      input_raw ic >>= function
      | None    -> Lwt.return_none
      | Some "" -> niet
      | Some s  ->
        let size = String.length s in
        if s.[size - 1] <> Git_misc.lf then err_no_trailing_lf ();
        let s = String.with_range s ~len:(size-1) in
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

    let pp_request = Fmt.of_to_string string_of_request

    type t = {
      request: request;
      discover: bool; (* The smart HTTP protocol has 2 modes. *)
      gri: Git_gri.t;
    }

    let host t = Uri.host (Git_gri.to_uri t.gri)
    let uri t = Git_gri.to_uri t.gri

    (* Initialisation sentence for the Git protocol *)
    let git t =
      let uri = Git_gri.to_uri t.gri in
      let message =
        let buf = Buffer.create 1024 in
        let path = match Uri.path uri with "" -> "/" | p  -> p in
        Buffer.add_string buf (string_of_request t.request);
        Buffer.add_char   buf Git_misc.sp;
        Buffer.add_string buf path;
        Buffer.add_char   buf Git_misc.nul;
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
            Buffer.add_char buf Git_misc.nul;
        end;
        Buffer.contents buf in
      PacketLine.string_of_line message

    let ssh t =
      Printf.sprintf "%s %s" (string_of_request t.request)
        (Uri.path (Git_gri.to_uri t.gri))

    let smart_http t =
      (* Note: GitHub wants User-Agent to start by `git/`  *)
      let useragent = "User-Agent", ogit_agent in
      let headers : (string * string) list =
        if t.discover
        then [useragent]
        else [
          useragent;
          "Content-Type", Printf.sprintf "application/x-%s-request"
            (string_of_request t.request);
        ]
      in
      Marshal.to_string headers []

    let to_string t =
      match protocol_exn (Git_gri.to_uri t.gri) with
      | `Git -> Some (git t)
      | `SSH -> Some (ssh t)
      | `Smart_HTTP -> Some (smart_http t)

    let create request ~discover gri =
      Log.debug (fun l ->
          l "Init.create request=%a discover=%b gri=%s"
            pp_request request discover (Git_gri.to_string gri));
      let protocol = protocol_exn (Git_gri.to_uri gri) in
      let gri = match protocol with
        | `SSH | `Git -> gri
        | `Smart_HTTP ->
          let service = if discover then "info/refs?service=" else "" in
          let url = Git_gri.to_string gri in
          let request = string_of_request request in
          Git_gri.of_string (Printf.sprintf "%s/%s%s" url service request)
      in
      Log.debug (fun l -> l "computed-gri: %s" (Git_gri.to_string gri));
      { request; discover; gri }

    let upload_pack = create Upload_pack
    let receive_pack = create Receive_pack
    let _upload_archive = create Upload_archive
  end


  module Listing = struct

    include Listing

    let input ic protocol =
      Log.debug (fun l -> l "Listing.input (protocol=%a)" pp_protocol protocol);
      let error fmt = error ("[SMART-HTTP] Listing.input:" ^^ fmt) in
      let skip_smart_http () =
        match protocol with
        | `Git | `SSH -> Lwt.return_unit
        | `Smart_HTTP ->
          PacketLine.input ic >>= function
          | None      -> error "missing # header."
          | Some line ->
            match String.cut line ~sep:Git_misc.sp_str with
            | Some ("#", service) ->
              Log.debug (fun l -> l "skipping %s" service);
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
          match String.cut line ~sep:Git_misc.sp_str with
          | Some ("ERR", err) -> error "ERROR: %s" err
          | Some (h, r)  ->
            let h = Hash_IO.Commit.of_hex h in
            if is_empty acc then (
              (* Read the capabilities on the first line *)
              match String.cut r ~sep:Git_misc.nul_str with
              | Some (r, caps) ->
                let r = Git_reference.of_raw r in
                let h = Git_hash.of_commit h in
                let hashes = Git_hash.Map.add_multi h r acc.hashes in
                let references = Git_reference.Map.add r h acc.references in
                let capabilities = Capabilities.of_string caps in
                aux { hashes; capabilities; references }
              | None ->
                let r = Git_reference.of_raw r in
                let h = Git_hash.of_commit h in
                let hashes = Git_hash.Map.add_multi h r acc.hashes in
                let references = Git_reference.Map.add r h acc.references in
                aux { hashes; references; capabilities = [] }
            ) else
              let r = Git_reference.of_raw r in
              let h = Git_hash.of_commit h in
              let hashes = Git_hash.Map.add_multi h r acc.hashes in
              let references = Git_reference.Map.add r h acc.references in
              aux { acc with hashes; references }
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
      | Ack_multi of Git_hash.t * status
      | Ack of Git_hash.t
      | Nak

    let input ic =
      Log.debug (fun l -> l "Ack.input");
      PacketLine.input ic >>= function
      | None
      | Some "NAK" -> Lwt.return Nak
      | Some s      ->
        match String.cut s ~sep:Git_misc.sp_str with
        | Some ("ACK", r) ->
          begin match String.cut r ~sep:Git_misc.sp_str with
            | None         -> Lwt.return (Ack (Hash_IO.of_hex r))
            | Some (id, s) ->
              Lwt.return (Ack_multi (Hash_IO.of_hex id, status_of_string s))
          end
        | _ -> error "%S invalid ack" s

    let _inputs ic =
      Log.debug (fun l -> l "Ack.inputs");
      let rec aux acc =
        input ic >>= function
        | Nak -> Lwt.return (List.rev (Nak :: acc))
        | tok -> aux (tok :: acc)
      in
      aux []

  end

  module Upload_request = struct

    type message =
      | Want of Git_hash.Commit.t * Capability.t list
      | Shallow of Git_hash.t
      | Deepen of int
      | Unshallow of Git_hash.t
      | Have of Git_hash.Commit.t
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
      Log.debug (fun l -> l "Upload.input");
      let rec aux acc =
        PacketLine.input_raw ic >>= function
        | None   -> Lwt.return (List.rev acc)
        | Some l ->
          match String.cut l ~sep:Git_misc.sp_str with
          | None -> error "input upload"
          | Some (kind, s) ->
            match kind with
            | "shallow"   -> aux (Shallow (Hash_IO.of_hex s) :: acc)
            | "unshallow" -> aux (Unshallow (Hash_IO.of_hex s) :: acc)
            | "have"      -> aux (Have (Hash_IO.Commit.of_hex s) :: acc)
            | "done"      -> aux (Done :: acc)
            | "deepen"    ->
              let d =
                try int_of_string s
                with Failure _ -> err_invalid_integer "Upload.input" s
              in
              aux (Deepen d :: acc)
            | "want" ->
              let aux id c = aux (Want (Hash_IO.Commit.of_hex id, c) :: acc) in
              begin match String.cut s ~sep:Git_misc.sp_str with
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
      Log.debug (fun l -> l "Upload.output");

      (* output wants *)
      Lwt_list.iteri_s (fun i (id, c) ->
          if i = 0 && c <> [] then
            (* first-want *)
            let msg = Printf.sprintf
                "want %s %s\n" (Git_hash.Commit.to_hex id) (Capabilities.to_string c)
            in
            PacketLine.output_line oc msg
          else
            (* additional-want *)
            let msg = Printf.sprintf "want %s\n" (Git_hash.Commit.to_hex id) in
            if i <> 0 && c <> [] then
              Log.warn (fun l ->
                  l "additional-want: ignoring %a." Capabilities.pp c);
            PacketLine.output_line oc msg
        ) (filter_wants t)
      >>= fun () ->

      (* output shallows *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "shallow %s" (Git_hash.to_hex id) in
          PacketLine.output_line oc msg
        ) (filter_shallows t)
      >>= fun () ->

      (* output unshallows *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "unshallow %s" (Git_hash.to_hex id) in
          PacketLine.output_line oc msg
        ) (filter_unshallows t)
      >>= fun () ->

      (* output haves *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "have %s\n" (Git_hash.Commit.to_hex id) in
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
      shallows: Git_hash.t list;
      unshallows: Git_hash.t list;
    }

    (* PHASE1: the client send the the IDs he wants, the sever answers with
       the new shallow state. *)
    let phase1 (ic, oc) ?deepen ~capabilities ~shallows ~wants =
      Log.debug (fun l -> l "Upload.phase1");
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

    let kind c = match Char.to_int c with
      | 1 -> Pack
      | 2 -> Progress
      | 3 -> Fatal
      | i -> error "Side_band: %d is not a valid message type" i

    exception Error of string

    let input ?(progress=fun _ -> ()) ic =
      Log.debug (fun l -> l "Side_band.input");
      let size = ref 0 in
      let t0 = Sys.time () in
      let pp s =
        size := !size + String.length s;
        let mib = 1024. *. 1024. in
        let total = float_of_int !size /. mib in
        let per_s = total /. (Sys.time () -. t0) in
        let done_ = if s = "" then ", done.\n" else "" in
        let str =
          Printf.sprintf "Receiving objects: %.2f MiB | %.2f MiB/s%s"
            total per_s done_
        in
        progress str
      in
      let rec aux acc =
        PacketLine.input_raw ic >>= function
        | None    -> pp ""; Lwt.return (List.rev acc)
        | Some "" -> aux acc
        | Some s  ->
          let payload = String.with_range s ~first:1 in
          pp payload;
          match kind s.[0] with
          | Pack     -> aux (payload :: acc)
          | Fatal    -> Lwt.fail (Error payload)
          | Progress ->
            let payload = "remote: " ^ payload in
            Log.info (fun l -> l "%s" payload);
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

    module Pack_IO = Git_pack.IO(Store.Digest)(Store.Inflate)

    type command =
      | Create of Git_reference.t * Git_hash.Commit.t
      | Delete of Git_reference.t * Git_hash.Commit.t
      | Update of Git_reference.t * Git_hash.Commit.t * Git_hash.Commit.t

    let pp_command ppf t =
      let r = Git_reference.pp in
      let c = Git_hash.Commit.pp in
      match t with
      | Create (name, new_id)         ->
        Fmt.pf ppf "create %a %a" r name c new_id
      | Delete (name, old_id)         ->
        Fmt.pf ppf "delete %a %a" r name c old_id
      | Update (name, old_id, new_id) ->
        Fmt.pf ppf "update %a %a %a" r name c old_id c new_id

    let pp_commands l = Fmt.(list ~sep:(const string " & ") pp_command) l

    let output_command buf t =
      let zero = Hash_IO.Commit.zero in
      let old_id, new_id, name = match t with
        | Create (name, new_id) -> zero, new_id, name
        | Delete (name, old_id) -> old_id, zero, name
        | Update (name, old_id, new_id) -> old_id, new_id, name in
      Printf.bprintf buf "%s %s %s"
        (Git_hash.Commit.to_hex old_id)
        (Git_hash.Commit.to_hex new_id)
        (Git_reference.to_raw name)

    type t = {
      capabilities: Capabilities.t;
      commands: command list;
      pack: Git_pack.t;
    }

    let pp ppf t =
      Fmt.pf ppf "UPDATE_REQUEST:\n%a\n%a\npack: %d"
        Capabilities.pp t.capabilities
        pp_commands t.commands
        (List.length t.pack)

    let output oc t =
      let rec aux first = function
        | []   -> PacketLine.flush oc
        | x::y ->
          let buf = Buffer.create 1024 in
          output_command buf x;
          if first then (
            Buffer.add_char buf Git_misc.nul;
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
          Log.info (fun l -> l "SENDING: %d bytes" len);
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
        begin match String.cut line ~sep:Git_misc.sp_str with
          | Some ("unpack", "ok") -> Lwt.return `Ok
          | Some ("unpack", err ) -> Lwt.return (`Error err)
          | _ -> Lwt.fail (Failure "Report_status.input: unpack-status")
        end >>= fun result ->
        let aux acc =
          PacketLine.input ic >>= function
          | None      -> Lwt.return acc
          | Some line ->
            match String.cut line ~sep:Git_misc.sp_str with
            | Some ("ok", name)  ->
              Lwt.return ((Git_reference.of_raw name, `Ok) :: acc)
            | Some ("ng", cont)  ->
              begin match String.cut cont ~sep:Git_misc.sp_str with
                | None  -> Lwt.fail (Failure "Report_status.input: command-fail")
                | Some (name, err) ->
                  Lwt.return ((Git_reference.of_raw name, `Error err) :: acc)
              end
            | _ -> Lwt.fail (Failure "Report_status.input: command-status")
        in
        aux [] >>= fun commands ->
        Lwt.return { Result.result; commands }

  end

  let todo msg =
    Lwt.fail (Failure ("TODO: " ^ msg))

  type fetch = {
    haves       : Git_hash.t list;
    shallows    : Git_hash.t list;
    deepen      : int option;
    unpack      : bool;
    capabilities: Capabilities.t;
    wants       : want list option;
    clone       : bool;
  }

  type op =
    | Ls
    | Fetch of fetch

  module Graph = Git_graph.Make(Store)
  module Pack_IO = Git_pack.IO(Store.Digest)(Store.Inflate)

  let push ?ctx t ~branch gri =
    Log.debug (fun l -> l "Sync.push");
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
          Log.debug (fun l -> l "listing:\n %a" Listing.pp listing);
          Store.read_reference t branch    >>= fun new_obj ->
          let old_obj = Listing.find_reference listing branch in
          let commit = Git_hash.to_commit in
          let command = match old_obj, new_obj with
            | None  , None   -> err_unknown_tag branch
            | Some x, None   -> Update_request.Delete (branch, commit x)
            | None  , Some x -> Update_request.Create (branch, commit x)
            | Some x, Some y -> Update_request.Update (branch, commit x, commit y)
          in
          let capabilities =
            `Report_status :: match command with
            | Update_request.Delete _ -> [`Delete_refs]
            | _                       -> [`Ofs_delta ]
          in
          let commands = [ command ] in
          let min =
            Git_hash.Map.keys (Listing.hashes listing) |> Git_hash.Set.of_list
          in
          let max = match new_obj with
            | None   -> Git_hash.Set.empty
            | Some x -> Git_hash.Set.singleton x
          in
          Graph.pack t ~min ~max >>= fun values ->
          let pack = Pack_IO.create values in
          let request = { Update_request.capabilities; commands; pack } in
          Log.debug (fun l -> l "request:\n%a" Update_request.pp request);
          Update_request.output oc request >>= fun () ->
          Report_status.input ic
        )

  let fetch_commits t (ic, oc) ?(progress=fun _ -> ()) f listing wants =
    Log.debug (fun l ->
        l "Sync.fetch_commits %a" (Fmt.list Git_hash.Commit.pp) wants);
    let f =
      let server_caps = Listing.capabilities listing in
      (* The client MUST NOT ask for capabilities the server did not
         say it supports. *)
      let capabilities = Capabilities.restrict f.capabilities server_caps in
      { f with capabilities }
    in
    let haves =
      let server_tips =
        Git_hash.Map.keys (Listing.hashes listing)
        |> Lwt_list.filter_p (Store.mem t)
        >|= Git_hash.Set.of_list
      in
      server_tips >>= fun server_tips ->
      let client_tips = Git_hash.Set.of_list f.haves in
      Graph.closure t ~full:false ~min:server_tips ~max:client_tips >|= fun g ->
      Graph.keys g |> List.map Git_hash.to_commit
    in
    let wants =
      let w = Git_hash.Commit.Set.of_list wants in
      let h = Git_hash.Commit.Set.of_list (List.map Git_hash.to_commit f.haves) in
      Git_hash.Commit.Set.diff w h
      |> Git_hash.Commit.Set.to_list
    in
    if wants = [] then (
      Log.debug (fun l ->
          l "Nothing to want: nothing to do! skip the pack file read.");
      progress "Already up-to-date.\n";
      Lwt.return { Result.listing; hashes = Git_hash.Set.empty }
    ) else (
      Log.debug (fun l -> l "PHASE1");
      let deepen = f.deepen in
      let capabilities = f.capabilities in
      let shallows = f.shallows in
      Upload_request.phase1 (ic, oc) ?deepen ~capabilities
        ~shallows ~wants
      >>= fun _phase1 ->

      (* XXX: process the shallow / unshallow.  *)
      (* XXX: need a notion of shallow/unshallow in API. *)

      Log.debug (fun l -> l "PHASE2");
      haves >>= fun haves ->
      Upload_request.phase2 (ic,oc) ~haves >>= fun () ->

      Log.debug (fun l -> l "PHASE3");
      progress "Receiving data ...\n";
      Pack_file.input ~capabilities ~progress ic >>= fun bufs ->

      let size = List.fold_left (fun acc s -> acc + String.length s) 0 bufs in
      Log.info (fun l -> l "Received a pack file of %d bytes." size);
      let pack = Cstruct.create size in
      let _size = List.fold_left (fun acc buf ->
          let len = String.length buf in
          Cstruct.blit_from_string buf 0 pack acc len;
          acc + len
        ) 0 bufs in

      Log.debug (fun l -> l "unpack=%b" f.unpack);
      let read = Store.read_inflated t in
      Pack_IO.Raw.input ~progress ~read (Mstruct.of_cstruct pack) >>= fun pack ->
      let unpack () =
        if f.unpack || Git_pack.Raw.shallow pack then
          Pack_IO.Raw.unpack ~progress ~write:(Store.write_inflated t) pack
        else
          Store.write_pack t pack
      in
      unpack () >>= fun hashes ->
      match Git_hash.Set.cardinal hashes with
      | 0 ->
        Log.debug (fun l -> l "No new objects");
        progress "Already up-to-date.\n";
        Lwt.return { Result.listing; hashes }
      | n ->
        Log.debug (fun l -> l "%d new objects" n);
        Lwt.return { Result.listing; hashes }
    )

  let write_heads_and_tags t r h =
    if is_head_or_tag r then
      Store.mem t h >>= function
      | false -> Lwt.return_unit
      | true  -> Store.write_reference t r h
    else
      Lwt.return_unit

  (* Query the remote store for its references and its HEAD. *)
  let with_listing ?ctx gri k =
    Log.debug (fun l -> l "Sync.with_listing");
    let init = Init.upload_pack ~discover:true gri in
    match Init.host init with
    | None   -> todo "local-clone"
    | Some _ ->
      let uri = Init.uri init in
      let protocol = protocol_exn uri in
      let init = Init.to_string init in
      IO.with_connection ?ctx uri ?init (fun (ic, oc) ->
          Listing.input ic protocol >>= fun listing ->
          Log.debug (fun l -> l "listing:\n %a"  Listing.pp listing);
          k (protocol, ic, oc) listing
        )

  let err_sha1_not_advertised h =
    err
      "Cannot fetch %a as the server does not advertise \
       'allow-reachable-sha1-in-want' and it is not in the \
       list of head commits advertised by `upload-pack`."
      Git_hash.pp h

  let fetch_pack ?ctx ?progress t gri op =
    with_listing ?ctx gri (fun (protocol, ic, oc) listing ->
        match op with
        | Ls      -> Lwt.return { Result.listing; hashes = Git_hash.Set.empty }
        | Fetch f ->
          let references = Listing.references listing in
          let references, commits = match f.wants with
            | None   ->
              (* We ask for all the remote references *)
              Git_reference.Map.fold (fun r c (rs, cs as acc) ->
                  if not (is_head_or_tag r) then acc
                  else (r, c) :: rs, Git_hash.Set.add c cs
                ) references ([], Git_hash.Set.empty)
            | Some wants ->
              let allow_sha1 =
                let caps = Listing.capabilities listing in
                let all = List.mem `Allow_reachable_sha1_in_want caps in
                fun h ->
                  all ||
                  let hashes = Listing.hashes listing in
                  try List.exists is_head_or_tag (Git_hash.Map.find h hashes)
                  with Not_found -> false
              in
              List.fold_left (fun (rs, cs as acc) -> function
                  | `Commit c ->
                    let c = Git_hash.of_commit c in
                    if allow_sha1 c then
                      let refs =  Listing.find_hash listing c in
                      let rs = List.map (fun r -> (r, c)) refs @ rs in
                      rs, Git_hash.Set.add c cs
                    else err_sha1_not_advertised c
                  | `Ref r    ->
                    try
                      let c = Git_reference.Map.find r references in
                      (r, c) :: rs, Git_hash.Set.add c cs
                    with Not_found ->
                      acc
                ) ([], Git_hash.Set.empty) wants
          in
          let commits = Git_hash.Set.to_list commits |> List.map Git_hash.to_commit in
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
            if not f.clone then Lwt.return_unit
            else
              Lwt_list.iter_p
                (fun (r, c) -> write_heads_and_tags t r c)
                references
          in
          sync ()        >>= fun r ->
          update_refs () >|= fun () ->
          r
      )

  let ls ?ctx t gri =
    Log.debug (fun l -> l "ls %s" (Git_gri.to_string gri));
    fetch_pack ?ctx t gri Ls >|= fun r ->
    Result.references r

  let fetch_aux ~clone
      ?ctx ?deepen ?(unpack=false) ?(capabilities=Capabilities.default)
      ?wants ?progress t gri =
    let op = if clone then "clone" else "fetch" in
    Log.debug (fun l ->
        l "%s %s wants=%a" op (Git_gri.to_string gri) pp_wants wants);
    Store.references t >>= fun refs ->
    Lwt_list.fold_left_s (fun haves r ->
        Store.read_reference t r >|= function
        | None   -> haves
        | Some h -> Git_hash.Set.add h haves
      ) Git_hash.Set.empty refs
    >>= fun commits ->
    let haves = Git_hash.Set.to_list commits in
    (* XXX: Store.shallows t >>= fun shallows *)
    let shallows = [] in
    let op = { shallows; haves; deepen; unpack; capabilities; wants; clone } in
    fetch_pack ?ctx ?progress t gri (Fetch op)

  let fetch = fetch_aux ~clone:false

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
          progress
            (Printf.sprintf "HEAD is now at %s\n" (Git_hash.Commit.to_hex head));
          Lwt.return_unit
    in
    update_head () >>= update_checkout

  let clone ?ctx ?deepen ?unpack ?capabilities ?branch ?progress t
      ~checkout gri =
    let wants = match branch with
      | None   -> None
      | Some b -> Some [b]
    in
    fetch_aux ~clone:true ?ctx ?deepen ?unpack ?capabilities ?wants ?progress
      t gri >>= fun result ->
    let head = match branch with
      | None              -> None
      | Some (`Ref b)    -> Some (Git_reference.Ref b)
      | Some (`Commit c) -> Some (Git_reference.Hash c)
    in
    populate ?head ?progress t ~checkout result >|= fun () -> result

  type t = Store.t

end

module type S = sig
  type t
  type ctx
  val ls: ?ctx:ctx -> t -> Git_gri.t -> Git_hash.t Git_reference.Map.t Lwt.t
  val push: ?ctx:ctx -> t -> branch:Git_reference.t -> Git_gri.t -> Result.push Lwt.t
  val fetch:
    ?ctx:ctx ->
    ?deepen:int ->
    ?unpack:bool ->
    ?capabilities:capability list ->
    ?wants:want list ->
    ?progress:(string -> unit) ->
    t -> Git_gri.t -> Result.fetch Lwt.t
  val clone:
    ?ctx:ctx ->
    ?deepen:int ->
    ?unpack:bool ->
    ?capabilities:capability list ->
    ?branch:want ->
    ?progress:(string -> unit) ->
    t -> checkout:bool -> Git_gri.t -> Result.fetch Lwt.t
end
