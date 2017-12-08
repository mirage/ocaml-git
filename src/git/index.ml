(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
 * and Romain Calascibetta <romain.calascibetta@gmail.com>
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

let src = Logs.Src.create "git.idx" ~doc:"Git index decoder"
module Log = (val Logs.src_log src : Logs.LOG)

module Int32 = struct
  include Int32

  let ( << ) = Int32.shift_left (* >> (tuareg) *)
  let ( || ) = Int32.logor
  let ( && ) = Int32.logand
end

module MakeIndexDecoder (H: S.HASH) = struct

  module Hash = H

  type error =
    | Invalid_byte of int
    | Invalid_mode of int32 * int32
    | Invalid_hash of Hash.t * Hash.t
    | Unrecognized_extension of string
    | Malformed_extension

  let pp_error ppf = function
    | Invalid_byte x             -> Fmt.pf ppf "(Invalid_byte %02x)" x
    | Invalid_mode (kind, perm)  -> Fmt.pf ppf "(Invalid_mode (kind:%lx, perm:%lx))" kind perm
    | Invalid_hash (expect, has) -> Fmt.pf ppf "(Invalid_hash (expect:%a, has:%a))"
                                      Hash.pp expect Hash.pp has
    | Unrecognized_extension ext -> Fmt.pf ppf "(Unrecognized_extension %s)" ext
    | Malformed_extension        -> Fmt.string ppf "Malformed_extension"

  type kind = Normal | Exec | Symlink | Gitlink

  let pp_kind ppf = function
    | Normal -> Fmt.string ppf "Normal"
    | Exec -> Fmt.string ppf "Exec"
    | Symlink -> Fmt.string ppf "Symlink"
    | Gitlink -> Fmt.string ppf "Gitlink"

  type time =
    { lsb32 : int32
    ; nsec  : int32 }

  let pp_time ppf { lsb32; nsec; } =
    Fmt.pf ppf "{ @[<hov>lsb32 = %ld;@ \
                nsec = %ld;@] }"
      lsb32 nsec

  type 'e flag =
    { assume : bool
    ; extend : 'e option
    ; stage  : int
    ; length : int }

  let pp_flag pp_ext ppf { assume; extend; stage; length; } =
    Fmt.pf ppf "{ @[<hov>assume = %b;@ \
                extend = %a;@ \
                stage = %d;@ \
                length = %d;@] }"
      assume (Fmt.hvbox (Fmt.Dump.option pp_ext)) extend stage length

  type extend =
    { reserved      : bool
    ; skip_worktree : bool
    ; intent_to_add : bool }

  let pp_extend ppf { reserved; skip_worktree; intent_to_add; } =
    Fmt.pf ppf "{ @[<hov>reserved = %b;@ \
                skip_worktree = %b;@ \
                intent_to_add = %b;@] }"
      reserved skip_worktree intent_to_add

  type info =
    { ctime : time
    ; mtime : time
    ; dev   : int32
    ; ino   : int32
    ; mode  : kind
    ; uid   : int32
    ; gid   : int32
    ; size  : int32 }

  let pp_info ppf { ctime; mtime; dev; ino; mode; uid; gid; size; } =
    Fmt.pf ppf "{ @[<hov>ctime = %a;@ \
                mtime = %a;@ \
                dev = %lx;@ \
                ino = %ld;@ \
                mode = %a;@ \
                uid = %ld;@ \
                gid = %ld;@ \
                size = %ld;@] }"
      (Fmt.hvbox pp_time) ctime
      (Fmt.hvbox pp_time) mtime
      dev ino
      pp_kind mode
      uid gid size


  type entry =
    { info  : info
    ; hash  : Hash.t
    ; flag  : extend flag
    ; path  : Fpath.t }

  let pp_entry ppf { info; hash; flag; path; } =
    Fmt.pf ppf "{ @[<hov>info = %a;@ \
                hash = %a;@ \
                flag = %a;@ \
                path = %a;@] }"
      (Fmt.hvbox pp_info) info
      (Fmt.hvbox Hash.pp) hash
      (Fmt.hvbox (pp_flag pp_extend)) flag
      (Fmt.hvbox Fpath.pp) path

  type index = entry list

  let pp_index = Fmt.Dump.list pp_entry

  module Ext =
  struct
    type signature = int * int * int * int

    let signature_from_string x =
      let get s i = Char.code (String.unsafe_get s i) in

      if String.length x = 4
      then (get x 0, get x 1, get x 2, get x 3)
      else raise (Invalid_argument (Fmt.strf "Invalid signature: %S." x))

    type cached_tree =
      { path    : Fpath.t
      ; covered : int
      ; subtree : int
      ; hash    : Hash.t }

    let pp_cached_tree ppf { path; covered; subtree; hash; } =
      Fmt.pf ppf "{ @[<hov>path = %a;@ \
                  covered = %d;@ \
                  subtree = %d;@ \
                  hash = %a;@] }"
        Fpath.pp path covered subtree Hash.pp hash

    type resolve_undo =
      { path    : Fpath.t
      ; stages  : (mode * Hash.t) list }
    and mode = int

    let pp_resolve_undo ppf { path; stages; } =
      Fmt.pf ppf "{ @[<hov>path = %a;@ \
                  stages = %a;@] }"
        Fpath.pp path
        Fmt.(hvbox (Dump.list (Dump.pair int Hash.pp))) stages

    type ewah = unit (* TODO *)

    let pp_ewah = Fmt.nop

    type untracked =
      { environments : string list
      ; exclude      : (info * Hash.t) option * (info * Hash.t) option
      ; gitignore    : Fpath.t
      ; entries      : directory list }
    and directory =
      { untracked    : string list
      ; subdirectory : int
      ; name         : string }

    type value = ..
    type value += Cached_tree of cached_tree list
    type value += Resolve_undo of resolve_undo list
    type value += Split_index of { shared : Hash.t; deleted : ewah; replaced : ewah; }
    type value += Untracked of untracked

    let pp_value ppf = function
      | Cached_tree lst ->
        Fmt.pf ppf "(Cached_tree %a)" (Fmt.hvbox (Fmt.Dump.list pp_cached_tree)) lst
      | Resolve_undo lst ->
        Fmt.pf ppf "(Resolve_undo %a)" (Fmt.hvbox (Fmt.Dump.list pp_resolve_undo)) lst
      | Split_index { shared; deleted; replaced; } ->
        Fmt.pf ppf "(Split_index { @{<hov>shared = %a;@ \
                    deleted = %a;@ \
                    replaced = %a;@] })"
          Hash.pp shared
          (Fmt.hvbox pp_ewah) deleted
          (Fmt.hvbox pp_ewah) replaced
      | Untracked { environments; exclude = (exclude, core); gitignore; entries; } ->
        let pp_entry ppf { untracked; subdirectory; name; } =
          Fmt.pf ppf "{ @[<hov>untracked = %a;@ \
                      subdirectory = %d;@ \
                      name = %s;@] }"
            Fmt.(hvbox (Dump.list string)) untracked
            subdirectory
            name
        in

        Fmt.pf ppf "{ @[<hov>environements = %a;@ \
                    info/exclude = %a;@ \
                    core.exclude ) %a;@ \
                    gitignore = %a;@ \
                    entires = %a;@] }"
          Fmt.(hvbox (Dump.list string)) environments
          Fmt.(hvbox (Dump.option (pair pp_info Hash.pp))) exclude
          Fmt.(hvbox (Dump.option (pair pp_info Hash.pp))) core
          Fpath.pp gitignore
          Fmt.(hvbox (Dump.list pp_entry)) entries
      | _ -> Fmt.pf ppf "(Un-pretty-printable extension value)"
  end

  type t =
    { i_off      : int
    ; i_pos      : int
    ; i_len      : int
    ; read       : int
    ; version    : int32
    ; many       : int32
    ; rest       : int32
    ; entries    : entry list
    ; hash       : Hash.Digest.ctx
    ; extensions : Ext.value list (* XXX(dinosaure): should be a set. *)
    ; state      : state }
  and k = Cstruct.t -> t -> res
  and state =
    | Header of k
    | Entry of k
    | Padding of int
    | Signature of k
    | Extension of int * (int -> k) extension
    | Hash of Hash.t
    | End
    | Exception of error
  and res =
    | Wait of t
    | Error of t * error
    | Cont of t
    | Ok of t * index * Ext.value list
  and 'k extension = ..
  constraint 'k = int -> k

(* XXX(dinosaure): we use the open type only for the main developper.
   That means, we don't let the user to put a new extension in the
   decoder. It's just /more easy/ to integrate an extension in the
   decoder.

   In fact, this is consist to define the extension value in {!Ext},
   make a decoder which has the type [k] (see [constraint 'k = k]),
   add new constructor in the [extension] type and add the signature
   to the [extensions] hash-table.

   Then, the decoder resolve alone which parser to use depending on
   the signature. You should return an [Ext.value] and the decoder put
   it internally in [t.extensions].

   We does not allow the user to add a new extension because he needs
   to interact directly with the internal state [t] (in the [k]
   function). So, we prefer to let the main developper to manipulate
   this state and to do not expose the definition of the state (which
   should be abstract in any case). *)

type 'k extension += Tree of 'k
type 'k extension += Reuc of 'k
type 'k extension += Link of 'k
type 'k extension += Untr of 'k

let pp_extension ppf = function
  | Tree _ -> Fmt.string ppf "(Tree #k)"
  | Reuc _ -> Fmt.string ppf "(Reuc #k)"
  | Link _ -> Fmt.string ppf "(Link #k)"
  | Untr _ -> Fmt.string ppf "(Untr #k)"
  | _ -> Fmt.string ppf "(Un-pretty-printable extension)"

let pp_state ppf = function
  | Header _      -> Fmt.string ppf "(Header #k)"
  | Entry _       -> Fmt.string ppf "(Entry #k)"
  | Padding n     -> Fmt.pf ppf "(Padding %d)" n
  | Signature _   -> Fmt.string ppf "(Signature #k)"
  | Extension (rest, ext) -> Fmt.pf ppf "(Extension (rest:%d, %a))" rest (Fmt.hvbox pp_extension) ext
  | Hash hash     -> Fmt.pf ppf "(Hash %a)" Hash.pp hash
  | End           -> Fmt.string ppf "End"
  | Exception err -> Fmt.pf ppf "(Exception %a)" (Fmt.hvbox pp_error) err

let pp ppf t =
  Fmt.pf ppf "{ @[<hov>i_off = %d;@ \
              i_pos = %d;@ \
              i_len = %d;@ \
              read = %d;@ \
              entries = %a;@ \
              extensions = %a;@ \
              state = %a;@] }"
    t.i_off t.i_pos t.i_len t.read
    (Fmt.hvbox pp_index) t.entries
    (Fmt.hvbox (Fmt.Dump.list Ext.pp_value)) t.extensions
    (Fmt.hvbox pp_state) t.state

let digest_and_await src t : res =
  let () = Hash.Digest.feed t.hash (Cstruct.sub src t.i_off t.i_len) in
  Wait t

let await _ t : res =
  Wait t

let error exn t : res =
  Error ({ t with state = Exception exn }, exn)

let rec get_byte ?(digest = true) ~ctor k src t =
  if (t.i_len - t.i_pos) > 0
  then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
    k byte src
      { t with i_pos = t.i_pos + 1
             ; read = t.read + 1 }
  else
    let t' = { t with state = ctor (fun src t -> (get_byte[@tailcall]) ~ctor k src t) } in
    if digest then digest_and_await src t' else await src t

(* XXX(dinosaure): The semantic of this function is clear but could
   fail. Indeed, [peek_byte] returns [Some byte] when the current
   input is not empty. Otherwise, it asks __only one time__ to refill
   the input to the user. If the user refill again with an empty
   input, [peek_byte] decides to return [None] (and probably terminate
   the decoding).

   This semantic could fail if we are in this situation:
   -> decode input
   -> wait more input when k = peek_byte
   -> refill by empty input (but the input __is not__ completely consumed)
   -> a next refill can returns a filled input or an empty input again

      at this stage, the decoder does not care about the last refill
      and just finalize the decoding.

   In reality, this is does not appear when we read __a file__. [read
   ic == 0] means the end of the file. But can appear on a socket
   ([read socket == 0] means we don't have __yet__ input). So, this
   decoder can not be use on a [socket], only on a file.

   Fortunately, the index data is a file in reality.
*)
let peek_byte ~ctor k src t =
  let next src t =
    if (t.i_len - t.i_pos) > 0
    then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
      k (Some byte) src t
    else k None src t
  in

  if (t.i_len - t.i_pos) > 0
  then let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
    k (Some byte) src t
  else await src { t with state = ctor (fun src t -> next src t) }

let from_lsb32 b0 b1 b2 b3 =
  let open Int32 in
  (of_int b0 << 24) (* >> (tuareg) *)
  || (of_int b1 << 16) (* >> (tuareg) *)
  || (of_int b2 << 8)  (* >> (tuareg) *)
  || (of_int b3)
[@@warning "-44"]

let from_lsb16 b0 b1 =
  (b0 lsl 8) lor b1

let rec get_lsb32 ?(digest = true) ~ctor k src t =
  let get_byte = get_byte ~digest ~ctor in

  if (t.i_len - t.i_pos) > 3
  then let num = Cstruct.BE.get_uint32 src (t.i_off + t.i_pos) in
    k num src
      { t with i_pos = t.i_pos + 4
             ; read = t.read + 4 }
  else if (t.i_len - t.i_pos) > 0
  then
    (get_byte
     @@ fun byte0 -> get_byte
     @@ fun byte1 -> get_byte
     @@ fun byte2 -> get_byte
     @@ fun byte3 src t ->
     k (from_lsb32 byte0 byte1 byte2 byte3) src t) src t
  else
    let t' = { t with state = ctor (fun src t -> (get_lsb32[@tailcall]) ~ctor k src t) } in
    if digest then digest_and_await src t' else await src t

let rec get_lsb16 ?(digest = true) ~ctor k src t =
  let get_byte = get_byte ~digest ~ctor in

  if (t.i_len - t.i_pos) > 1
  then let num = Cstruct.BE.get_uint16 src (t.i_off + t.i_pos) in
    k num src
      { t with i_pos = t.i_pos + 2
             ; read = t.read + 2 }
  else if (t.i_len - t.i_pos) > 0
  then
    (get_byte
     @@ fun byte0 -> get_byte
     @@ fun byte1 src t -> k (from_lsb16 byte0 byte1) src t) src t
  else
    let t' = { t with state = ctor (fun src t -> (get_lsb16[@tailcall]) ~ctor k src t) } in
    if digest then digest_and_await src t' else await src t'

let get_hash ?digest ~ctor k src t =
  let buf = Buffer.create Hash.Digest.length in
  let get_byte = get_byte ?digest ~ctor in

  let rec go pos src t =
    if pos = Hash.Digest.length
    then k (Hash.of_string (Buffer.contents buf)) src t
    else
      get_byte
        (fun byte src t -> Buffer.add_char buf (Char.chr byte); (go[@tailcall]) (pos + 1) src t)
        src t
  in

  go 0 src t

let to_nul ?digest ~ctor k src t =
  let get_byte = get_byte ?digest ~ctor in
  let buf = Buffer.create 16 in

  let rec go src t =
    get_byte (fun byte src t ->
        if byte = 0
        then k (Buffer.contents buf) src t
        else begin
          Buffer.add_char buf (Char.unsafe_chr byte);
          go src t
        end)
      src t
  in

  go src t

module KHeader =
struct
  let get_byte = get_byte ~digest:true ~ctor:(fun k -> Header k)
  let get_lsb32 = get_lsb32 ~digest:true ~ctor:(fun k -> Header k)

  let check_byte byte k src t =
    get_byte (fun byte' src t -> if byte = byte' then k src t else error (Invalid_byte byte') t) src t
end

module KEntry =
struct
  let get_byte = get_byte ~digest:true ~ctor:(fun k -> Entry k)
  let get_lsb32 = get_lsb32 ~digest:true ~ctor:(fun k -> Entry k)
  let get_lsb16 = get_lsb16 ~digest:true ~ctor:(fun k -> Entry k)
  let get_hash = get_hash ~digest:true ~ctor:(fun k -> Entry k)
  let to_nul = to_nul ~digest:true ~ctor:(fun k -> Entry k)

  let get_time k src t =
    (get_lsb32
     @@ fun lsb32 -> get_lsb32
     @@ fun nsec src t -> k { lsb32; nsec; } src t)
      src t

  let get_kind k src t =
    (get_lsb32
     @@ fun mode src t -> match Int32.(mode && 0xF0000000l), Int32.(mode && 0x1FFl) with
     | 0b1000l, 0o755l -> k Exec src t
     | 0b0000l, 0o755l -> k Exec src t
     | 0b1000l, 0o644l -> k Normal src t
     | 0b0000l, 0o644l -> k Normal src t
     | 0b1010l, 0o000l -> k Symlink src t
     | 0b1110l, 0o000l -> k Gitlink src t
     | kind, perm -> error (Invalid_mode (kind, perm)) t)
      src t

  let to_bool = function
    | 0 -> false
    | 1 -> true
    | _ -> assert false (* XXX(dinosaure): we mask value *)

  let get_flag k src t =
    (get_lsb16
     @@ fun flag src t -> match flag land 0x8000, flag land 0x4000, flag land 0x3000, flag land 0x0FFF with
     | assume, extend, stage, length ->
       k { assume = to_bool assume
         ; extend = Some (to_bool extend)
         ; stage
         ; length } src t)
      src t

  let get_extend k src t =
    (get_lsb16
     @@ fun extend src t -> match extend land 0x8000, extend land 0x4000, extend land 0x2000 with
     | reserved, skip_worktree, intent_to_add ->
       k { reserved = to_bool reserved
         ; skip_worktree = to_bool skip_worktree
         ; intent_to_add = to_bool intent_to_add } src t)
      src t

  let take len k src t =
    let buf = Buffer.create len in

    let rec go pos src t =
      get_byte (fun byte src t ->
          if pos = len
          then k (Buffer.contents buf) src t
          else begin
            Buffer.add_char buf (Char.unsafe_chr byte);
            go (pos + 1) src t
          end)
        src t
    in

    go 0 src t

  let get_varint k src t =
    let rec go byte n src t =
      if byte land 128 = 0
      then k n src t
      else
        let n = n + 1 in
        let n = (n lsl 7) + (byte land 127) in
        get_byte (fun byte src t ->
            go byte ((n lsl 7) + (byte land 127)) src t)
          src t
    in

    get_byte (fun byte src t -> go byte (byte land 127) src t) src t
end

module KSignature =
struct
  let get_byte = get_byte ~ctor:(fun k -> Signature k)
  let get_lsb32 = get_lsb32 ~ctor:(fun k -> Signature k)
  let peek_byte = peek_byte ~ctor:(fun k -> Signature k)
end

module KExtension =
struct
  (* Common functions lifted with [rest] byte(s). *)

  let rec get_byte ~ext k rest src t =
    if rest = 0
    then error Malformed_extension t
    else if (t.i_len - t.i_pos) = 0
    then digest_and_await src { t with state = Extension (rest, ext (fun rest src t -> (get_byte[@tailcall]) ~ext k rest src t)) }
    else
      let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
      k byte (rest - 1) src
        { t with i_pos = t.i_pos + 1
               ; read = t.read + 1 }

  let rec peek_byte ~ext k rest src t =
    if rest = 0
    then k None rest src t
    else if (t.i_len - t.i_pos) = 0
    then digest_and_await src { t with state = Extension (rest, ext (fun rest src t -> (peek_byte[@tailcall]) ~ext k rest src t)) }
    else
      let byte = Cstruct.get_uint8 src (t.i_off + t.i_pos) in
      k (Some byte) rest src t

  let to_nul ~ext k rest src t =
    let get_byte = get_byte ~ext in
    let buf = Buffer.create 16 in

    let rec go rest src t =
      get_byte (fun byte rest src t ->
          if byte = 0
          then k (Buffer.contents buf) rest src t
          else begin
            Buffer.add_char buf (Char.unsafe_chr byte);
            go rest src t
          end)
        rest src t
    in

    go rest src t

  let check_byte ~ext byte k rest src t =
    let get_byte = get_byte ~ext in

    get_byte (fun byte' rest src t ->
        if byte = byte' then k rest src t else error (Invalid_byte byte') t)
      rest src t

  let option_map f = function Some x -> Some (f x) | None -> None

  let get_digit ~ext k rest src t =
    let buf = Buffer.create 10 in
    let peek_byte = peek_byte ~ext in

    let rec go rest src t =
      (peek_byte
       @@ fun byte rest src t -> match option_map Char.unsafe_chr byte with
       | Some ('0' .. '9' as chr) ->
         Buffer.add_char buf chr;
         go (rest - 1) src { t with i_pos = t.i_pos + 1
                                  ; read = t.read + 1 }
       | Some _ ->
         k (int_of_string (Buffer.contents buf)) rest src t
       | None ->
         k (int_of_string (Buffer.contents buf)) rest src t)
      rest src t
    in

    go rest src t

  let get_octal ~ext k rest src t =
    let buf = Buffer.create 10 in
    let peek_byte = peek_byte ~ext in

    let rec go rest src t =
      (peek_byte
       @@ fun byte rest src t -> match option_map Char.unsafe_chr byte with
       | Some ('0' .. '7' as chr) ->
         Buffer.add_char buf chr;
         go (rest - 1) src { t with i_pos = t.i_pos + 1
                                  ; read = t.read + 1 }
       | Some _ ->
         k (int_of_string ("0o" ^ Buffer.contents buf)) rest src t
       | None ->
         k (int_of_string ("0o" ^ Buffer.contents buf)) rest src t)
      rest src t
    in

    go rest src t

  let get_hash ~ext k rest src t =
    let buf = Buffer.create Hash.Digest.length in
    let get_byte = get_byte ~ext in

    let rec go pos rest src t =
      if pos = Hash.Digest.length
      then k (Hash.of_string (Buffer.contents buf)) rest src t
      else
        get_byte
          (fun byte rest src t -> Buffer.add_char buf (Char.chr byte); (go[@tailcall]) (pos + 1) rest src t)
          rest src t
    in

    go 0 rest src t

  module KTree =
  struct
    let get_byte = get_byte ~ext:(fun k -> Tree k)
    let to_nul = to_nul ~ext:(fun k -> Tree k)
    let space = check_byte ~ext:(fun k -> Tree k) (Char.code ' ')
    let lf = check_byte ~ext:(fun k -> Tree k) (Char.code '\n')
    let get_digit = get_digit ~ext:(fun k -> Tree k)
    let get_hash = get_hash ~ext:(fun k -> Tree k)

    let get_entry k rest src t =
      (to_nul
       @@ fun path -> get_digit
       @@ fun covered -> space @@ get_digit
       @@ fun subtree -> lf @@ get_hash
       @@ fun hash rest src t -> k { Ext.path = Fpath.v path; covered; subtree; hash; } rest src t)
      rest src t
  end

  module KReuc =
  struct
    let get_byte = get_byte ~ext:(fun k -> Reuc k)
    let to_nul = to_nul ~ext:(fun k -> Reuc k)
    let get_octal = get_octal ~ext:(fun k -> Reuc k)
    let nul = check_byte ~ext:(fun k -> Reuc k) 0
    let get_hash = get_hash ~ext:(fun k -> Reuc k)

    let get_entry k rest src t =
      (to_nul
       @@ fun path -> get_octal
       @@ fun mode0 -> nul @@ get_octal
       @@ fun mode1 -> nul @@ get_octal
       @@ fun mode2 -> nul @@ fun rest src t ->
       let k = match mode0, mode1, mode2 with
         | 0o0, 0o0, 0o0 ->
           k { Ext.path = Fpath.v path; stages = [] }
         | mode0, 0o0, 0o0 ->
           get_hash @@ fun hash0 ->
           k { Ext.path = Fpath.v path; stages = [ mode0, hash0 ] }
         | mode0, mode1, 0o0 ->
           get_hash @@ fun hash0 ->
           get_hash @@ fun hash1 ->
           k { Ext.path = Fpath.v path; stages = [ mode0, hash0; mode1, hash1 ] }
         | mode0, mode1, mode2 ->
           get_hash @@ fun hash0 ->
           get_hash @@ fun hash1 ->
           get_hash @@ fun hash2 ->
           k { Ext.path = Fpath.v path; stages = [ mode0, hash0; mode1, hash1; mode2, hash2 ] }
       in k rest src t)
        rest src t
  end
end

let path info hash flag src t =
  (* XXX(dinosaure): Why? Git optimizes cache entries. Indeed,
     adjacent cache entries tend to share the leading paths:
     - foo/bar
     - foo/baz

     So it makes sense to only store the differences in later
     entries. In the v4 on-disk format of the index, each on-disk
     entry stores the number of bytes to be stripped from the end of
     the previous name, and the bytes to append to the result, to
     come up with its name.

     For v3 and earlier, previous-name should be None in anytime of
     the deserialization. *)

  let previous_name =
    if t.version <= 3l
    then None
    else match t.entries with
      | [] -> None
      | { path; _ } :: _ -> Some (Fpath.to_string path)
      (* XXX(dinosaure): we are not sure the current entry want to
         split on '/', so it's better to use [String.concat] instead
         [Fpath.append]. *)
  in

  match previous_name with
  | Some previous_name ->
    (KEntry.get_varint
     @@ fun len' ->
     let lead = String.sub previous_name (String.length previous_name - len') len' in
     KEntry.to_nul
     @@ fun rest _ t ->
     let entry = { info; hash; flag; path = Fpath.v (lead ^ rest) } in
     Cont { t with entries = entry :: t.entries
                 ; state = Padding 1 })
      src t
  | None ->
    if flag.length = 0xFFF
    then (KEntry.to_nul
          @@ fun name _ t ->
          let entry = { info; hash; flag = { flag with length = String.length name; }; path = Fpath.v name } in
          Cont { t with entries = entry :: t.entries
                      ; state = Padding 1 })
        src t
    else (KEntry.take flag.length
          @@ fun name _ t ->
          let entry = { info; hash; flag; path = Fpath.v name } in
          Cont { t with entries = entry :: t.entries
                      ; state = Padding 1 })
        src t

let entry src t =
  (KEntry.get_time
   @@ fun ctime -> KEntry.get_time
   @@ fun mtime -> KEntry.get_lsb32
   @@ fun dev   -> KEntry.get_lsb32
   @@ fun ino   -> KEntry.get_kind
   @@ fun mode  -> KEntry.get_lsb32
   @@ fun uid   -> KEntry.get_lsb32
   @@ fun gid   -> KEntry.get_lsb32
   @@ fun size  -> KEntry.get_hash
   @@ fun hash  -> KEntry.get_flag
   @@ fun flag  ->

   let info =
     { ctime
     ; mtime
     ; dev
     ; ino
     ; mode
     ; uid
     ; gid
     ; size }
   in

   (match flag.extend with
    | Some true ->
      KEntry.get_extend (fun extend -> path info hash { flag with extend = Some extend })
    | Some false | None ->
      path info hash { flag with extend = (None : extend option) }))
    src t

let cstruct_copy cs =
  let ln = Cstruct.len cs in
  let rs = Cstruct.create ln in
  Cstruct.blit cs 0 rs 0 ln;
  rs

let extensions = Hashtbl.create 10

let rec signature src t =
  let get_signature src t =
    (KSignature.get_byte
     @@ fun byte0 -> KSignature.get_byte ~digest:false
     @@ fun byte1 -> KSignature.get_byte ~digest:false
     @@ fun byte2 -> KSignature.get_byte ~digest:false
     @@ fun byte3 ->

     Log.debug (fun l -> l ~header:"signature" "Get the signature: %c%c%c%c."
                   (Char.unsafe_chr byte0)
                   (Char.unsafe_chr byte1)
                   (Char.unsafe_chr byte2)
                   (Char.unsafe_chr byte3));

     try match Hashtbl.find extensions (byte0, byte1, byte2, byte3) with
       | Tree k -> KSignature.get_lsb32 ~digest:true (fun length -> k (Int32.to_int length))
       | Reuc k -> KSignature.get_lsb32 ~digest:true (fun length -> k (Int32.to_int length))
       | Link k -> KSignature.get_lsb32 ~digest:true (fun length -> k (Int32.to_int length))
       | Untr k -> KSignature.get_lsb32 ~digest:true (fun length -> k (Int32.to_int length))
       | _ ->
         let consumed = Bytes.create 4 in
         Bytes.set consumed 0 (Char.unsafe_chr byte0);
         Bytes.set consumed 1 (Char.unsafe_chr byte1);
         Bytes.set consumed 2 (Char.unsafe_chr byte2);
         Bytes.set consumed 3 (Char.unsafe_chr byte3);

         Hash.Digest.feed t.hash (Cstruct.of_bytes consumed);

         (* XXX(dinosaure): you need to read the comment below to
            understand why we did not digest bytes and we digest
            now. *)

         KSignature.get_lsb32 ~digest:true (fun x -> move (Int32.to_int x))
     with Not_found ->
       Log.debug (fun l -> l ~header:"signature" "Signature %c%c%c%c not found."
                     (Char.unsafe_chr byte0)
                     (Char.unsafe_chr byte1)
                     (Char.unsafe_chr byte2)
                     (Char.unsafe_chr byte3));

       let consumed = Bytes.create 4 in
       Bytes.set consumed 0 (Char.unsafe_chr byte0);
       Bytes.set consumed 1 (Char.unsafe_chr byte1);
       Bytes.set consumed 2 (Char.unsafe_chr byte2);
       Bytes.set consumed 3 (Char.unsafe_chr byte3);

       (* XXX(dinosaure): at this stage, the specification does not
          explain which input could be mark the end of the index file.
          It's an alteration between signature and resulting hash.

          That means, we need to get 4 bytes, find if it's an existing
          extension. The problem is: the 4 bytes could be a
          non-specified (in the code) signature. So we continue to say
          this is a possible extension.

          Then, we have an 32-bits integer (if it's an extension)
          which informs the length of the extension. Finally, we ask
          to the user more than [Hash.Digest.length - 8] bytes. If we
          have more than [Hash.Digest.length - 8] bytes, it's not the
          end of the index file and we are, indeed in an extension
          data (and we will just give up the input to the next stage).

          Otherwise, that means [byte0, byte1, byte2, byte3] and bytes
          from the 32-bits integer is a part of the resulting hash
          (it's why we don't digest them). So we get the rest,
          concatene them with first 8 bytes and finally compare with
          the resulting hash.

          The INDEX file is a shit btw. The alteration can found below
          with [end] and we can not distinct [extension*] and [hash]
          without any advance.

          index-file := 'DIRC', version = int32, n = int32, entries = list(n, entry), end

          end :=
            | extension*
            | hash

          extension := signature = int32, length = int32, data(length)
          entry := ..

          data length := length byte(s)
          list length p :=
            if length = 0 then [] else p :: list (length - 1) p
       *)

       KSignature.get_lsb32 ~digest:false (move_or_die (Bytes.unsafe_to_string consumed)))
      src t
  in

  get_signature src t
and move length src t =
  match length with
  | 0 -> signature src t
  | n ->
    if t.i_len - t.i_pos > 0
    then
      let n' = min n (t.i_len - t.i_pos) in
      move (length - n') src { t with i_pos = t.i_pos + n'
                                    ; read = t.read + n' }
    else digest_and_await src t
and move_or_die a b src t =
  let consumed = Cstruct.create 8 in
  Cstruct.blit_from_string a 0 consumed 0 4;
  Cstruct.BE.set_uint32 consumed 4 b;

  let consumed = Cstruct.concat
      [ consumed
      ; cstruct_copy (Cstruct.sub src (t.i_off + t.i_pos) (t.i_len - t.i_pos)) ]
  in

  let final consumed src t =
    if t.i_len - t.i_pos = 0
    then Cont { t with state = Hash (Hash.of_string (Cstruct.to_string consumed)) }
    else
      let () = Hash.Digest.feed t.hash consumed in
      move Int32.(to_int (sub b (of_int (Cstruct.len consumed - 8)))) src t
  in

  let rec consume_to_hash consumed src t =
    let has = t.i_len - t.i_pos in
    let expect_if_hash =
      let x = (Hash.Digest.length - (Cstruct.len consumed)) in
      if x >= 0 then x else 0
    in
    let expect_if_extension =
      let x = Int32.(sub b (of_int (Cstruct.len consumed - 8))) in
      if x >= 0l then x else 0l
    in

    Log.debug (fun l -> l ~header:"consume_to_hash" "We has %d byte(s), \
                                                     we expect %d byte(s) to hash, \
                                                     we expect %d byte(s) to extension."
                  has expect_if_hash (Int32.to_int expect_if_extension));

    if has < expect_if_hash
    then
      let consumed = Cstruct.concat
          [ consumed
          ; cstruct_copy (Cstruct.sub src (t.i_off + t.i_pos) (t.i_len - t.i_pos)) ]
      in

      await src { t with i_pos = t.i_pos + (t.i_len - t.i_pos)
                       ; read = t.read + (t.i_len - t.i_pos)
                       ; state = Signature (consume_to_hash consumed) }
    else if has = expect_if_hash
    then
      let consumed = Cstruct.concat
          [ consumed
          ; cstruct_copy (Cstruct.sub src (t.i_off + t.i_pos) (t.i_len - t.i_pos)) ] in

      (* XXX(dinosaure): last chance to refill the input with
         something. if it's empty, we consumed the hash, otherwise, it's
         sure, this is an extension data.

         However, this is the same semantic than [peek_byte]. That
         means, this is only work on a file. *)
      await src { t with i_pos = t.i_pos + (t.i_len - t.i_pos)
                       ; read = t.read + (t.i_len - t.i_pos)
                       ; state = Signature (final consumed) }
    else (* has > expect_if_hash *)
      let () = Hash.Digest.feed t.hash consumed in
      move (Int32.to_int expect_if_extension) src t
  in

  await src { t with i_pos = t.i_pos + (t.i_len - t.i_pos)
                   ; read = t.i_pos + (t.i_len - t.i_pos)
                   ; state = Signature (consume_to_hash consumed) }

let hash hash_expected _ t =
  let hash_resulting = Hash.Digest.get t.hash in
  if Hash.equal hash_expected hash_resulting
  then error (Invalid_hash (hash_expected, hash_resulting)) t
  else Cont { t with state = End }

let switch src t = match t.rest with
  | 0l -> Cont { t with state = (Signature signature) }
  | n -> entry src { t with rest = Int32.sub n 1l }

let rec tree acc rest src t =
  if rest = 0
  then
    let entries = List.rev acc in
    Cont { t with state = Signature signature
                ; extensions = (Ext.Cached_tree entries) :: t.extensions }
  else KExtension.KTree.get_entry (fun entry rest src t -> tree (entry :: acc) rest src t) rest src t

let rec reuc acc rest src t =
  if rest = 0
  then
    let entries = List.rev acc in
    Cont { t with state = Signature signature
                ; extensions = (Ext.Resolve_undo entries) :: t.extensions }
  else KExtension.KReuc.get_entry (fun entry rest src t -> reuc (entry :: acc) rest src t) rest src t

let link _ _ t = Cont t
let untr _ _ t = Cont t

let () = Hashtbl.add extensions (Ext.signature_from_string "TREE") (Tree (tree []))
let () = Hashtbl.add extensions (Ext.signature_from_string "REUC") (Reuc (reuc []))
let () = Hashtbl.add extensions (Ext.signature_from_string "link") (Link link)
let () = Hashtbl.add extensions (Ext.signature_from_string "UNTR") (Untr untr)

let nul = Cstruct.of_string "\000"

let padding n src t =
  let rec go n src t =
    let peek_byte = peek_byte ~ctor:(fun _ -> Padding n) in

    if n = 8 || t.version = 4l
    then Cont { t with state = Entry switch }
    else
      peek_byte
        (function
          | Some 0 -> fun src t ->
            let () = Hash.Digest.feed t.hash nul in

            go (n + 1) src { t with i_pos = t.i_pos + 1
                                  ; read = t.read + 1 }
          | _ -> fun _ t -> Cont { t with state = Entry switch })
        src t
  in

  go n src t
(* XXX(dinosaure): I don't know if I need to count the last \000 of
   the path or not. *)

let many src t =
  KHeader.get_lsb32
    (fun many _ t ->
       Cont { t with many
                   ; rest = many
                   ; state = Entry switch })
    src t

let version src t =
  KHeader.get_lsb32 (fun version src t -> many src { t with version }) src t

let header src t =
  (   KHeader.check_byte (Char.code 'D')
      @@ KHeader.check_byte (Char.code 'I')
      @@ KHeader.check_byte (Char.code 'R')
      @@ KHeader.check_byte (Char.code 'C') version)
    src t

let default =
  { i_off      = 0
  ; i_pos      = 0
  ; i_len      = 0
  ; read       = 0
  ; version    = 0l
  ; many       = 0l
  ; rest       = 0l
  ; entries    = []
  ; hash       = Hash.Digest.init ()
  ; extensions = []
  ; state      = Header header }

let eval src t =

  let eval0 t = match t.state with
    | Header k -> k src t
    | Entry k -> k src t
    | Padding n -> padding n src t
    | Signature k -> k src t
    | Extension (rest, Tree k) -> k rest src t
    | Extension (rest, Reuc k) -> k rest src t
    | Extension (rest, Link k) -> k rest src t
    | Extension (rest, Untr k) -> k rest src t
    | Extension (_, ext) -> Error (t, Unrecognized_extension (Fmt.strf "%a" pp_extension ext))
    | Hash hash' -> hash hash' src t
    | End -> Ok (t, t.entries, t.extensions)
    | Exception err -> Error (t, err)
  in

  let rec loop t = match eval0 t with
    | Cont t -> loop t
    | Error (t, exn) -> `Error (t, exn)
    | Ok (t, index, extensions) -> `End (t, index, extensions)
    | Wait t -> `Await t
  in

  loop t

let refill off len t =
  { t with i_off = off
         ; i_len = len
         ; i_pos = 0 }

let used_in t = t.i_pos
end

module Make (H: S.HASH) (FS: S.FS) = struct

  module Hash = H
  module FileSystem = FS
  module IndexDecoder = MakeIndexDecoder(H)

  type error =
    [ `IndexDecoder of IndexDecoder.error
    | `SystemFile of FileSystem.File.error ]

  let pp_error ppf = function
    | `IndexDecoder err -> Fmt.pf ppf "(`IndexDecoder %a)" IndexDecoder.pp_error err
    | `SystemFile err -> Fmt.pf ppf "(`SystemFile %a)" FileSystem.File.pp_error err

  let load ~root ~dtmp =
    let open Lwt.Infix in

    FileSystem.File.open_r ~mode:0o400 Fpath.(root / "index")[@warning "-44"]
    >>= function
    | Error sys_err ->
      Log.debug (fun l -> l "Retrieve a file-system error: %a." FileSystem.File.pp_error sys_err);
      Lwt.return (Error (`SystemFile sys_err))
    | Ok read ->
      let decoder = IndexDecoder.default in

      let rec loop decoder = match IndexDecoder.eval dtmp decoder with
        | `Error (_, err) -> Lwt.return (Error (`IndexDecoder err))
        | `End (_, index, extensions) -> Lwt.return (Ok (index, extensions))
        | `Await decoder ->
          FileSystem.File.read dtmp read >>= function
          | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
          | Ok n ->
            Log.debug (fun l -> l "Reading %d byte(s) of the file-descriptor" n);
            loop (IndexDecoder.refill 0 n decoder)
      in

      loop decoder
end
