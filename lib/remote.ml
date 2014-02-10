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

open Core_kernel.Std
open Lwt
module Log = Log.Make(struct let section = "remote" end)

let error fmt =
  Printf.ksprintf (fun msg ->
      Printf.printf "Protocol error: %s\n%!" msg;
      raise Parsing.Parse_error
    ) fmt

let lwt_error fmt =
  Printf.ksprintf (fun msg ->
      Printf.printf "Protocol error: %s\n%!" msg;
      fail Parsing.Parse_error
    ) fmt

type result = {
  head      : SHA1.Commit.t option;
  references: SHA1.Commit.t Reference.Map.t;
  sha1s     : SHA1.t list;
}

module type IO = sig
  type ic
  type oc
  val with_connection: string -> int option -> (ic * oc -> 'a Lwt.t) -> 'a Lwt.t
  val read_all: ic -> string Lwt.t
  val read_exactly: ic -> int -> string Lwt.t
  val write: oc -> string -> unit Lwt.t
  val flush: oc -> unit Lwt.t
end

module Make (IO: IO) (Store: Store.S) = struct

  module PacketLine = struct

    type t = string option

    let output oc = function
      | None  ->
        Log.debugf "SENDING: FLUSH";
        IO.write oc "0000" >>= fun () ->
        IO.flush oc
      | Some l ->
        Log.debugf "SENDING: %S" l;
        let size = Printf.sprintf "%04x" (4 + String.length l) in
        IO.write oc size >>= fun () ->
        IO.write oc l    >>= fun () ->
        IO.flush oc

    let output_line oc s =
      output oc (Some s)

    let flush oc =
      output oc None

    let input ic =
      Log.debugf "PacketLine.input";
      IO.read_exactly ic 4 >>= fun size ->
      match size with
      | "0000" ->
        Log.debugf "RECEIVED: FLUSH";
        return_none
      | size   ->
        let size = int_of_string ("0x" ^ size) - 4 in
        IO.read_exactly ic size >>= fun payload ->
        Log.debugf "RECEIVED: %S (%d)" payload size;
        if payload.[size - 1] = Misc.lf then
          return (Some (String.sub payload 0 (size-1)))
        else
          return (Some payload)

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
      List.map ~f:Capability.of_string (String.split str ~on:Misc.sp)

    let to_string l =
      String.concat ~sep:" " (List.map ~f:Capability.to_string l)

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
        Buffer.add_char   buf Misc.sp;
        Buffer.add_string buf (Address.path t.address);
        Buffer.add_char   buf Misc.nul;
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
            Buffer.add_char buf Misc.nul;
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
      references  : Reference.t list SHA1.Commit.Map.t;
    }

    let references t =
      t.references

    let empty = {
      capabilities = [];
      references   = SHA1.Commit.Map.empty;
    }

    let is_empty t =
      t.capabilities = [] && Map.is_empty t.references

    let head t =
      Map.fold
        ~f:(fun ~key ~data acc -> if List.mem data Reference.head then Some key else acc)
        ~init:None t.references

    let pretty t =
      let buf = Buffer.create 1024 in
      Printf.bprintf buf "CAPABILITIES:\n%s\n" (Capabilities.to_string t.capabilities);
      Printf.bprintf buf "\nREFERENCES:\n";
      Map.iter
        ~f:(fun ~key ~data ->
            List.iter ~f:(fun ref ->
                Printf.bprintf buf "%s %s\n%!" (SHA1.Commit.to_hex key) (Reference.to_string ref)
              ) data
          ) t.references;
      Buffer.contents buf

    let input ic =
      Log.debugf "Listing.input";
      let rec aux acc =
        PacketLine.input ic >>= function
        | None      -> return acc
        | Some line ->
          match String.lsplit2 line ~on:Misc.sp with
          | Some ("ERR", err) -> error "ERROR: %s" err
          | Some (sha1, ref)  ->
            if is_empty acc then (
              (* Read the capabilities on the first line *)
              match String.lsplit2 ref ~on:Misc.nul with
              | Some (ref, caps) ->
                let ref = Reference.of_string ref in
                let references =
                  Map.add_multi ~key:(SHA1.Commit.of_hex sha1) ~data:ref acc.references in
                let capabilities = Capabilities.of_string caps in
                aux { references; capabilities; }
              | None ->
                let ref = Reference.of_string ref in
                let references =
                  Map.add_multi ~key:(SHA1.Commit.of_hex sha1) ~data:ref acc.references in
                aux { references; capabilities = []; }
            ) else
              let ref = Reference.of_string ref in
              let references =
                Map.add_multi ~key:(SHA1.Commit.of_hex sha1) ~data:ref acc.references in
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
      | Ack_multi of SHA1.t * status
      | Ack of SHA1.t
      | Nak

    let input ic =
      Log.debugf "Ack.input";
      PacketLine.input ic >>= function
      | None
      | Some "NAK" -> return Nak
      | Some s      ->
        match String.lsplit2 s ~on:Misc.sp with
        | Some ("ACK", r) ->
          begin match String.lsplit2 r ~on:Misc.sp with
            | None         -> return (Ack (SHA1.of_hex r))
            | Some (id, s) -> return (Ack_multi (SHA1.of_hex id, status_of_string s))
          end
        | _ -> error "%S invalid ack" s

    let inputs ic =
      Log.debugf "Ack.inputs";
      let rec aux acc =
        input ic >>= function
        | Nak -> return (List.rev (Nak :: acc))
        | tok -> aux (tok :: acc)
      in
      aux []

  end

  module Upload = struct

    type message =
      | Want of SHA1.t * Capability.t list
      | Shallow of SHA1.t
      | Deepen of int
      | Unshallow of SHA1.t
      | Have of SHA1.t
      | Done

    type t = message list

    let filter fn l =
      List.fold_left ~f:(fun acc elt ->
          match fn elt with
          | None   -> acc
          | Some x -> x::acc
        ) ~init:[] l

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

    let create l = l

    let input ic =
      Log.debugf "Upload.input";
      let rec aux acc =
        PacketLine.input ic >>= function
        | None   -> return (List.rev acc)
        | Some l ->
          match String.lsplit2 l ~on:Misc.sp with
          | None -> error "input upload"
          | Some (kind, s) ->
            match kind with
            | "shallow"   -> aux (Shallow   (SHA1.of_hex s) :: acc)
            | "unshallow" -> aux (Unshallow (SHA1.of_hex s) :: acc)
            | "have"      -> aux (Have      (SHA1.of_hex s) :: acc)
            | "done"      -> aux (Done                      :: acc)
            | "deepen"    ->
              let d =
                try int_of_string s
                with _ -> error "%s is not a valid integer" s in
              aux (Deepen d :: acc)
            | "want" ->
              let aux id c = aux (Want (SHA1.of_hex id, c) :: acc) in
              begin match String.lsplit2 s ~on:Misc.sp with
                | Some (id,c) -> aux id (Capabilities.of_string c)
                | None        -> match acc with
                  | Want (_,c)::_ -> aux s c
                  | _             -> error "want without capacity"
              end
            | s -> error "%s is not a valid upload request." s
      in
      aux []

    (* XXX: handler multi_hack *)
    let output oc t =
      Log.debugf "Upload.output";
      let last_c = ref [] in

      (* output wants *)
      Lwt_list.iter_s (fun (id, c) ->
          if c = !last_c then
            let msg = Printf.sprintf "want %s\n" (SHA1.to_hex id) in
            PacketLine.output_line oc msg
          else
            let msg =
              Printf.sprintf "want %s %s\n" (SHA1.to_hex id) (Capabilities.to_string c) in
            last_c := c;
            PacketLine.output_line oc msg
        ) (filter_wants t)
      >>= fun () ->

      (* output shallows *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "shallow %s" (SHA1.to_hex id) in
          PacketLine.output_line oc msg
        ) (filter_shallows t)
      >>= fun () ->

      (* output unshallows *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "unshallow %s" (SHA1.to_hex id) in
          PacketLine.output_line oc msg
        ) (filter_unshallows t)
      >>= fun () ->

      (* output haves *)
      Lwt_list.iter_s (fun id ->
          let msg = Printf.sprintf "have %s\n" (SHA1.to_hex id) in
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
      if List.mem t Done then
        PacketLine.output_line oc "done"
      else
        PacketLine.flush oc

    type phase1_result = {
      shallows: SHA1.t list;
      unshallows: SHA1.t list;
    }

    (* PHASE1: the client send the the IDs he wants, the severs answer
       the new shallow state. *)
    let phase1 (ic, oc) ?deepen ~shallows ~wants =
      Log.debugf "Upload.phase1";
      let wants = List.map ~f:(fun id -> Want (id, Capabilities.default)) wants in
      let shallows = List.map ~f:(fun id -> Shallow id) shallows in
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
      let haves = List.map ~f:(fun id -> Have id) haves in
      aux haves >>= fun () ->
      Ack.input ic >>= fun _ack ->
      return_unit

  end

  let todo msg =
    fail (Failure ("TODO: " ^ msg))

  type clone = {
    bare  : bool;
    deepen: int option;
    unpack: bool;
  }

  type fetch = {
    haves   : SHA1.t list;
    shallows: SHA1.t list;
    deepen  : int option;
    unpack  : bool;
  }

  type op =
    | Ls
    | Fetch of fetch
    | Clone of clone

  let fetch_pack t address op =
    let r = Init.create address in
    match Init.host r with
    | None   -> todo "local-clone"
    | Some h ->
      IO.with_connection h (Init.port r)(fun (ic, oc) ->
          Init.output oc r     >>= fun () ->
          Listing.input ic     >>= fun listing ->
          Log.debugf "listing:\n %s" (Listing.pretty listing);
          let references =
            List.fold_left ~f:(fun acc (sha1, refs) ->
                List.fold_left
                  ~f:(fun acc ref -> Reference.Map.add acc ~key:ref ~data:sha1)
                  ~init:acc
                  refs
              ) ~init:Reference.Map.empty
              (Map.to_alist (Listing.references listing)) in
          let head = Listing.head listing in
          match op with
          | Ls      -> return { head; references; sha1s = [] }
          | Fetch _
          | Clone _ ->
            begin match Map.find references Reference.head with
              | None      -> return_unit
              | Some sha1 ->
                let contents = Reference.head_contents references sha1 in
                Store.write_head t contents
            end >>= fun () ->
            let write_ref (ref, sha1) = Store.write_reference t ref sha1 in
            let references = Map.remove references Reference.head in
            Misc.list_iter_p write_ref (Map.to_alist references) >>= fun () ->

            match head with
            | None      ->
              Init.close oc >>= fun () ->
              return { head; references; sha1s = [] }
            | Some head ->
              Log.debugf "PHASE1";
              let deepen = match op with
                | Clone { deepen }
                | Fetch { deepen } -> deepen
                |  _               -> None in
              let shallows = match op with
                | Fetch { shallows } -> shallows
                | _                  -> [] in
              Upload.phase1 ?deepen (ic,oc) ~shallows ~wants:[SHA1.of_commit head]
              >>= fun _phase1 ->
              (* XXX: process the shallow / unshallow.  *)
              (* XXX: need a notion of shallow/unshallow in API. *)

              Log.debugf "PHASE2";
              let haves = match op with
                | Fetch { haves } -> haves
                | _               -> [] in
              Upload.phase2 (ic,oc) ~haves >>= fun () ->

              Log.debugf "PHASE3";
              printf "Receiving data ...%!";
              IO.read_all ic >>= fun raw ->
              printf " done.\n%!";
              Log.debugf "Received a pack file of %d bytes." (String.length raw);
              let pack = Bigstring.of_string raw in

              let unpack = match op with
                | Clone { unpack }
                | Fetch { unpack } -> unpack
                | _                -> false in

              begin if unpack then
                  Pack.unpack ~write:(Store.write t) pack
                else
                  let pack = Pack.Raw.input (Mstruct.of_bigarray pack) ~index:None in
                  Store.write_pack t pack
              end >>= fun sha1s ->
              match SHA1.Set.to_list sha1s with
              | []    ->
                Log.debugf "NO NEW OBJECTS";
                Printf.printf "Already up-to-date.\n%!";
                return { head = Some head; references; sha1s = [] }
              | sha1s ->
                Log.debugf "NEW OBJECTS";
                printf "remote: Counting objects: %d, done.\n%!"
                  (List.length sha1s);
                let bare = match op with
                  | Clone { bare } -> bare
                  | _              -> true in
                if not bare then (
                  Log.debugf "EXPANDING THE FILESYSTEM";
                  return { head = Some head; references; sha1s }
                ) else (
                  Log.debugf "BARE REPOSITORY";
                  return { head = None; references; sha1s }
                )
        )

  let ls t address =
    fetch_pack t address Ls >>= function
      { references } -> return references

  let clone t ?(bare=false) ?deepen ?(unpack=false) address =
    fetch_pack t address (Clone { bare; deepen; unpack })

  let fetch t ?deepen ?(unpack=false) address =
    Store.list t >>= fun haves ->
    (* XXX: Store.shallows t >>= fun shallows *)
    let shallows = [] in
    fetch_pack t address (Fetch { shallows; haves; deepen; unpack })

  type t = Store.t

end

module type S = sig
  type t
  val ls: t -> string -> SHA1.Commit.t Reference.Map.t Lwt.t
  val clone: t -> ?bare:bool -> ?deepen:int -> ?unpack:bool -> string -> result Lwt.t
  val fetch: t -> ?deepen:int -> ?unpack:bool -> string -> result Lwt.t
end
