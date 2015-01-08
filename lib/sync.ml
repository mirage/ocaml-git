(*
 * Copyright (c) 2013-2014 Thomas Gazagnaire <thomas@gazagnaire.org>
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

open Lwt
open Printf

module Log = Log.Make(struct let section = "remote" end)

module Result = struct

  type fetch = {
    head      : SHA.Commit.t option;
    references: SHA.Commit.t Reference.Map.t;
    sha1s     : SHA.t list;
  }

  let pretty_fetch t =
    let buf = Buffer.create 1024 in
    bprintf buf "HEAD: %s\n" (match t.head with
        | None -> "<none>"
        | Some x -> SHA.Commit.to_hex x);
    Reference.Map.iter (fun key data ->
        bprintf buf "%s %s\n" (Reference.pretty key) (SHA.Commit.to_hex data)
      ) t.references;
    bprintf buf "Keys: %d\n" (List.length t.sha1s);
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

module type IO = sig
  type ic
  type oc
  val with_connection: Uri.t -> ?init:string -> (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
  val read_all: ic -> string Lwt.t
  val read_exactly: ic -> int -> string Lwt.t
  val write: oc -> string -> unit Lwt.t
  val flush: oc -> unit Lwt.t
end

type capability =
  [ `Multi_ack
  | `Thin_pack
  | `Side_band
  | `Side_band_64k
  | `Ofs_delta
  | `Shallow
  | `No_progress
  | `Include_tag
  | `Report_status
  | `Delete_refs
  | `Agent of string
  | `Other of string ]

module Make (IO: IO) (Store: Store.S) = struct

  exception Error

  let error fmt =
    Printf.ksprintf (fun msg ->
        Printf.eprintf "%s\n%!" msg;
        raise Error
      ) fmt

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

    let input_raw ic: t Lwt.t =
      Log.debug "PacketLine.input";
      IO.read_exactly ic 4 >>= fun size ->
      match size with
      | "0000" ->
        Log.debug "RECEIVED: FLUSH";
        return_none
      | size   ->
        let size =
          let str = "0x" ^ size in
          try int_of_string str - 4
          with _ -> error "%s is not a valid integer" str in
        IO.read_exactly ic size >>= fun payload ->
        Log.debug "RECEIVED: %S (%d)" payload size;
        return (Some payload)

    let input ic =
      input_raw ic >>= function
      | None    -> return_none
      | Some "" -> return (Some "")
      | Some s  ->
        let size = String.length s in
        if s.[size - 1] <> Misc.lf then
          error "input: the payload doesn't have a trailing LF";
        let s = String.sub s 0 (size-1) in
        return (Some s)

  end

  module Capability = struct

    type t = capability

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
      | x               ->
        match Misc.string_lsplit2 x ~on:'=' with
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
      | `Other x       -> x

    let is_valid_fetch: t -> bool = function
      | `Multi_ack
      | `Thin_pack
      | `Side_band
      | `Side_band_64k
      | `Ofs_delta
      | `Shallow
      | `No_progress
      | `Include_tag -> true
      | _ -> false

    let is_valid_push: t -> bool = function
      | `Ofs_delta
      | `Report_status
      | `Delete_refs -> true
      | _ -> false

    let ogit_agent = `Agent ("ogit/" ^ Version.current)

  end

  module Capabilities = struct

    type t = Capability.t list

    let of_string str =
      List.map Capability.of_string (Misc.string_split str ~on:Misc.sp)

    let to_string l =
      String.concat " " (List.map Capability.to_string l)

    let pretty l =
      String.concat ", " (List.map Capability.to_string l)

    let default = [
      Capability.ogit_agent;
      `Side_band_64k;
      `No_progress;
    ]

    let _is_valid_push = List.for_all Capability.is_valid_push
    let _is_valid_fetch = List.for_all Capability.is_valid_fetch

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
      gri: Gri.t;
    }

    let host t = Uri.host (Gri.to_uri t.gri)

    (* Initialisation sentence for the Git protocol *)
    let git t =
      let uri = Gri.to_uri t.gri in
      let message =
        let buf = Buffer.create 1024 in
        Buffer.add_string buf (string_of_request t.request);
        Buffer.add_char   buf Misc.sp;
        Buffer.add_string buf (Uri.path uri);
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
      sprintf "%s %s" (string_of_request t.request) (Uri.path (Gri.to_uri t.gri))

    (* XXX: as we don't support the smart HTTP protocol (yet) we fall
       back the default Git protocol. *)
    let to_string t =
      match Gri.mode t.gri with
      | `HTTP | `Git -> git t
      | `SSH         -> ssh t

    let close oc =
      PacketLine.flush oc

    let upload_pack gri = { request = Upload_pack; gri }
    let receive_pack gri = { request = Receive_pack; gri }
    let _upload_archive gri = { request = Upload_archive; gri }
  end

  module Listing = struct

    type t = {
      capabilities: Capabilities.t;
      references  : Reference.t list SHA.Commit.Map.t;
    }

    let references t =
      t.references

    let empty = {
      capabilities = [];
      references   = SHA.Commit.Map.empty;
    }

    let is_empty t =
      t.capabilities = [] && SHA.Commit.Map.is_empty t.references

    let find_reference t ref =
      SHA.Commit.Map.fold
        (fun key data acc -> if List.mem ref data then Some key else acc)
        t.references None

    let head t =
      find_reference t Reference.head

    let pretty t =
      let buf = Buffer.create 1024 in
      Printf.bprintf buf "CAPABILITIES:\n%s\n"
        (Capabilities.to_string t.capabilities);
      Printf.bprintf buf "\nREFERENCES:\n";
      SHA.Commit.Map.iter
        (fun key data ->
           List.iter (fun ref ->
               Printf.bprintf buf "%s %s\n%!"
                 (SHA.Commit.to_hex key) (Reference.pretty ref)
             ) data
        ) t.references;
      Buffer.contents buf

    let input ic =
      Log.debug "Listing.input";
      let rec aux acc =
        PacketLine.input ic >>= function
        | None      -> return acc
        | Some line ->
          match Misc.string_lsplit2 line ~on:Misc.sp with
          | Some ("ERR", err) -> error "ERROR: %s" err
          | Some (sha1, ref)  ->
            if is_empty acc then (
              (* Read the capabilities on the first line *)
              match Misc.string_lsplit2 ref ~on:Misc.nul with
              | Some (ref, caps) ->
                let ref = Reference.of_raw ref in
                let references =
                  SHA.Commit.Map.add_multi (SHA.Commit.of_hex sha1) ref acc.references
                in
                let capabilities = Capabilities.of_string caps in
                aux { references; capabilities; }
              | None ->
                let ref = Reference.of_raw ref in
                let references =
                  SHA.Commit.Map.add_multi (SHA.Commit.of_hex sha1) ref acc.references
                in
                aux { references; capabilities = []; }
            ) else
              let ref = Reference.of_raw ref in
              let references =
                SHA.Commit.Map.add_multi (SHA.Commit.of_hex sha1) ref acc.references
              in
              aux { acc with references }
          | None -> error "%s is not a valid answer" line
      in
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
      | Some "NAK" -> return Nak
      | Some s      ->
        match Misc.string_lsplit2 s ~on:Misc.sp with
        | Some ("ACK", r) ->
          begin match Misc.string_lsplit2 r ~on:Misc.sp with
            | None         -> return (Ack (SHA.of_hex r))
            | Some (id, s) -> return (Ack_multi (SHA.of_hex id, status_of_string s))
          end
        | _ -> error "%S invalid ack" s

    let _inputs ic =
      Log.debug "Ack.inputs";
      let rec aux acc =
        input ic >>= function
        | Nak -> return (List.rev (Nak :: acc))
        | tok -> aux (tok :: acc)
      in
      aux []

  end

  module Upload_request = struct

    type message =
      | Want of SHA.t * Capability.t list
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
        PacketLine.input ic >>= function
        | None   -> return (List.rev acc)
        | Some l ->
          match Misc.string_lsplit2 l ~on:Misc.sp with
          | None -> error "input upload"
          | Some (kind, s) ->
            match kind with
            | "shallow"   -> aux (Shallow   (SHA.of_hex s) :: acc)
            | "unshallow" -> aux (Unshallow (SHA.of_hex s) :: acc)
            | "have"      -> aux (Have      (SHA.of_hex s) :: acc)
            | "done"      -> aux (Done                     :: acc)
            | "deepen"    ->
              let d =
                try int_of_string s
                with _ -> error "%s is not a valid integer" s in
              aux (Deepen d :: acc)
            | "want" ->
              let aux id c = aux (Want (SHA.of_hex id, c) :: acc) in
              begin match Misc.string_lsplit2 s ~on:Misc.sp with
                | Some (id,c) -> aux id (Capabilities.of_string c)
                | None        -> match acc with
                  | Want (_,c)::_ -> aux s c
                  | _             -> error "want without capacity"
              end
            | s -> error "%s is not a valid upload request." s
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
                "want %s %s\n" (SHA.to_hex id) (Capabilities.to_string c)
            in
            PacketLine.output_line oc msg
          else
            (* additional-want *)
            let msg = Printf.sprintf "want %s\n" (SHA.to_hex id) in
            if i <> 0 && c <> [] then
              Log.warn "'additional-want' should have empty capabilities";
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
          return_unit
        ) (filter_haves t)
      >>= fun () ->

      (* output deepen *)
      let deepen = filter_deepen t in
      begin if deepen <> 0 then (
          let msg = Printf.sprintf "deepen %d" deepen in
          PacketLine.output_line oc msg;
        ) else
          return_unit
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
      let wants = List.map (fun id -> Want (id, capabilities)) wants in
      let shallows = List.map (fun id -> Shallow id) shallows in
      let deepen = match deepen with
        | None   -> []
        | Some d -> [Deepen d] in
      output oc (wants @ shallows @ deepen) >>= fun () ->
      if deepen <> [] then
        input ic >>= fun res ->
        let shallows = filter_shallows res in
        let unshallows = filter_unshallows res in
        return (Some { shallows; unshallows })
      else
        return_none

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
      return_unit

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

    let input ic =
      Log.debug "Side_band.input";
      let rec aux acc =
        PacketLine.input_raw ic >>= function
        | None    -> return (List.rev acc)
        | Some "" -> aux acc
        | Some s  ->
          let payload = String.sub s 1 (String.length s - 1) in
          match kind s.[0] with
          | Pack     -> aux (payload :: acc)
          | Progress -> Log.info "remote: %s" payload; aux acc
          | Fatal    -> fail (Error payload)
      in
      aux [] >>= fun bufs ->
      return (String.concat "" bufs)

  end

  module Pack_file = struct

    let input ~capabilities ic =
      if List.mem `Side_band_64k capabilities
      || List.mem `Side_band capabilities
      then Side_band.input ic
      else IO.read_all ic

  end

  module Update_request = struct

    type command =
      | Create of Reference.t * SHA.Commit.t
      | Delete of Reference.t * SHA.Commit.t
      | Update of Reference.t * SHA.Commit.t * SHA.Commit.t

    let pretty_command t =
      let r = Reference.pretty in
      let c = SHA.Commit.to_hex in
      match t with
      | Create (name, new_id)         -> sprintf "create %s %s" (r name) (c new_id)
      | Delete (name, old_id)         -> sprintf "delete %s %s" (r name) (c old_id)
      | Update (name, old_id, new_id) -> sprintf "update %s %s %s" (r name) (c old_id) (c new_id)

    let pretty_commands l =
      String.concat " & " (List.map pretty_command l)

    let output_command buf t =
      let old_id, new_id, name = match t with
        | Create (name, new_id) -> SHA.Commit.zero, new_id, name
        | Delete (name, old_id) -> old_id, SHA.Commit.zero, name
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
      let s = Misc.with_buffer (fun b -> Pack.add b t.pack) in
      Log.info "SENDING: %s" s;
      IO.write oc s

  end

  module Report_status = struct

    let input ic =
      PacketLine.input ic >>= function
      | None -> fail (Failure "Report_status.input: empty")
      | Some line ->
        begin match Misc.string_lsplit2 line ~on:Misc.sp with
          | Some ("unpack", "ok") -> return `Ok
          | Some ("unpack", err ) -> return (`Error err)
          | _ -> fail (Failure "Report_status.input: unpack-status")
        end >>= fun result ->
        let aux acc =
          PacketLine.input ic >>= function
          | None      -> return acc
          | Some line ->
            match Misc.string_lsplit2 line ~on:Misc.sp with
            | Some ("ok", name)  -> return ((Reference.of_raw name, `Ok) :: acc)
            | Some ("ng", cont)  ->
              begin match Misc.string_lsplit2 cont ~on:Misc.sp with
                | None  -> fail (Failure "Report_status.input: command-fail")
                | Some (name, err) -> return ((Reference.of_raw name, `Error err) :: acc)
              end
            | _ -> fail (Failure "Report_status.input: command-status")
        in
        aux [] >>= fun commands ->
        return { Result.result; commands }

  end

  let todo msg =
    fail (Failure ("TODO: " ^ msg))

  type clone = {
    c_deepen: int option;
    c_unpack: bool;
    c_capabilites: Capabilities.t;
  }

  type fetch = {
    f_haves   : SHA.t list;
    f_shallows: SHA.t list;
    f_deepen  : int option;
    f_unpack  : bool;
    f_capabilites: Capabilities.t;
  }

  type op =
    | Ls
    | Fetch of fetch
    | Clone of clone

  module Graph = Global_graph.Make(Store)

  let push t ~branch gri =
    let r = Init.receive_pack gri in
    match Init.host r with
    | None   -> todo "local-clone"
    | Some _ ->
      let uri = Gri.to_uri gri in
      let init = Init.to_string r in
      IO.with_connection uri ~init (fun (ic, oc) ->
          Listing.input ic                 >>= fun listing ->
          (* XXX: check listing.capabilities *)
          Log.debug "listing:\n %s" (Listing.pretty listing);
          Store.read_reference t branch    >>= fun new_obj ->
          let old_obj = Listing.find_reference listing branch in
          let command = match old_obj, new_obj with
            | None  , None   -> failwith (Reference.pretty branch ^ ": unknown tag")
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
            SHA.Commit.Map.keys (Listing.references listing)
            |> List.map SHA.of_commit
            |> SHA.Set.of_list
          in
          let max = match new_obj with
            | None   -> SHA.Set.empty
            | Some x -> SHA.Set.singleton (SHA.of_commit x)
          in
          Graph.pack t ~min max >>= fun pack ->
          let request = { Update_request.capabilities; commands; pack } in
          Log.debug "request:\n%s" (Update_request.pretty request);
          Update_request.output oc request >>= fun () ->
          Report_status.input ic
        )

  let fetch_pack t gri op =
    let r = Init.upload_pack gri in
    match Init.host r with
    | None   -> todo "local-clone"
    | Some _ ->
      let uri = Gri.to_uri gri in
      let init = Init.to_string r in
      IO.with_connection uri ~init (fun (ic, oc) ->
          Listing.input ic >>= fun listing ->
          Log.debug "listing:\n %s" (Listing.pretty listing);
          let references =
            List.fold_left (fun acc (sha1, refs) ->
                List.fold_left
                  (fun acc ref -> Reference.Map.add ref sha1 acc)
                  acc
                  refs
              ) Reference.Map.empty
              (SHA.Commit.Map.to_alist (Listing.references listing)) in
          let head = Listing.head listing in
          match op with
          | Ls      -> return { Result.head; references; sha1s = [] }
          | Fetch _
          | Clone _ ->
            begin
              try
                let sha1 = Reference.Map.find Reference.head references in
                let contents = Reference.head_contents references sha1 in
                Store.write_head t contents
              with Not_found ->
                return_unit
            end >>= fun () ->
            let write_ref (ref, sha1) =
              if Reference.is_valid ref then Store.write_reference t ref sha1
              else return_unit in
            let references = Reference.Map.remove Reference.head references in
            Lwt_list.iter_p write_ref (Reference.Map.to_alist references)
            >>= fun () ->

            match head with
            | None      ->
              Init.close oc >>= fun () ->
              return { Result.head; references; sha1s = [] }
            | Some head ->
              Log.debug "PHASE1";
              let deepen = match op with
                | Clone { c_deepen = d; _ }
                | Fetch { f_deepen = d; _ } -> d
                |  _               -> None
              in
              let shallows = match op with
                | Fetch { f_shallows = s; _ } -> s
                | _ -> []
              in
              let capabilities = match op with
               | Fetch { f_capabilites = c; _ }
                | Clone { c_capabilites = c; _ } -> c
                | _ -> []
              in
              Upload_request.phase1 (ic,oc) ?deepen ~capabilities
                ~shallows ~wants:[SHA.of_commit head]
              >>= fun _phase1 ->

              (* XXX: process the shallow / unshallow.  *)
              (* XXX: need a notion of shallow/unshallow in API. *)

              Log.debug "PHASE2";
              let haves = match op with
                | Fetch { f_haves = h; _ } -> h
                | _ -> [] in
              Upload_request.phase2 (ic,oc) ~haves >>= fun () ->

              Log.debug "PHASE3";
              printf "Receiving data ...%!";
              Pack_file.input ~capabilities ic >>= fun raw ->

              printf " done.\n%!";
              Log.debug "Received a pack file of %d bytes." (String.length raw);
              let pack = Cstruct.of_string raw in

              let unpack = match op with
                | Clone { c_unpack = u; _ }
                | Fetch { f_unpack = u; _ } -> u
                | _ -> false in
              Log.debug "unpack=%b" unpack;

              begin if unpack then
                  Pack.unpack ~write:(Store.write ?level:None t) pack
                else
                  let pack = Pack.Raw.input (Mstruct.of_cstruct pack) ~index:None in
                  Store.write_pack t pack
              end >>= fun sha1s ->
              match SHA.Set.to_list sha1s with
              | []    ->
                Log.debug "NO NEW OBJECTS";
                Printf.printf "Already up-to-date.\n%!";
                return { Result.head = Some head; references; sha1s = [] }
              | sha1s ->
                Log.debug "NEW OBJECTS";
                printf "remote: Counting objects: %d, done.\n%!"
                  (List.length sha1s);
                return { Result.head = Some head; references; sha1s }
        )

  let ls t gri =
    fetch_pack t gri Ls >>= function
      { Result.references; _ } -> return references

  let clone t ?deepen ?(unpack=false) ?(capabilities=Capabilities.default) gri =
    let op = {
      c_deepen = deepen;
      c_unpack = unpack;
      c_capabilites = capabilities;
    } in
    fetch_pack t gri (Clone op)

  let fetch t ?deepen ?(unpack=false) ?(capabilities=Capabilities.default) gri =
    Store.list t >>= fun haves ->
    (* XXX: Store.shallows t >>= fun shallows *)
    let shallows = [] in
    let op = {
      f_shallows = shallows;
      f_haves = haves;
      f_deepen = deepen;
      f_unpack = unpack;
      f_capabilites = capabilities;
    } in
    fetch_pack t gri (Fetch op)

  type t = Store.t

end

module type S = sig
  type t
  val ls: t -> Gri.t -> SHA.Commit.t Reference.Map.t Lwt.t
  val push: t -> branch:Reference.t -> Gri.t -> Result.push Lwt.t
  val clone: t -> ?deepen:int -> ?unpack:bool -> ?capabilities:capability list
    -> Gri.t -> Result.fetch Lwt.t
  val fetch: t -> ?deepen:int -> ?unpack:bool -> ?capabilities:capability list
    -> Gri.t -> Result.fetch Lwt.t
end
