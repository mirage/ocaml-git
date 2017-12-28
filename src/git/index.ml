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
  let ( >> ) = Int32.shift_right
  let ( || ) = Int32.logor
  let ( && ) = Int32.logand
end

module Option =
struct
  let is_some = function Some _ -> true | None -> false
  let map f = function Some v -> Some (f v) | None -> None
end

module Entry (H: S.HASH) = struct

  module Hash = H

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

end

module MakeIndexDecoder (H: S.HASH) = struct

  module Hash = H
  module Entry = Entry(H)

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
      ; exclude      : (Entry.info * Hash.t) option * (Entry.info * Hash.t) option
      ; gitignore    : Fpath.t
      ; entries      : directory list }
    and directory =
      { untracked    : string list
      ; subdirectory : int
      ; name         : string }

    type value = ..
    type value += Cached_tree of [ `Entries of cached_tree list | `Invalid ]
    type value += Resolve_undo of resolve_undo list
    type value += Split_index of { shared : Hash.t; deleted : ewah; replaced : ewah; }
    type value += Untracked of untracked

    let pp_value ppf = function
      | Cached_tree state ->
        Fmt.pf ppf "(Cached_tree %a)"
          (fun ppf -> function
           | `Entries lst -> (Fmt.hvbox (Fmt.Dump.list pp_cached_tree)) ppf lst
           | `Invalid -> Fmt.pf ppf "#invalidState")
          state
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
          Fmt.(hvbox (Dump.option (pair Entry.pp_info Hash.pp))) exclude
          Fmt.(hvbox (Dump.option (pair Entry.pp_info Hash.pp))) core
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
    ; entries    : Entry.entry list
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
    | Ok of t * Entry.index * Ext.value list
  and 'k extension = ..

  (* XXX(dinosaure): we use the open type only for the main
     developper. That means, we don't let the user to put a new
     extension in the decoder. It's just /more easy/ to integrate an
     extension in the decoder.

     In fact, this is consist to define the extension value in {!Ext},
     make a decoder which has the type [k] (see [constraint 'k = k]),
     add new constructor in the [extension] type and add the signature
     to the [extensions] hash-table.

     Then, the decoder resolve alone which parser to use depending on
     the signature. You should return an [Ext.value] and the decoder
     put it internally in [t.extensions].

     We does not allow the user to add a new extension because he
     needs to interact directly with the internal state [t] (in the
     [k] function). So, we prefer to let the main developper to
     manipulate this state and to do not expose the definition of the
     state (which should be abstract in any case). *)

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
      (Fmt.hvbox Entry.pp_index) t.entries
      (Fmt.hvbox (Fmt.Dump.list Ext.pp_value)) t.extensions
      (Fmt.hvbox pp_state) t.state

  module Log =
  struct
    let src = Logs.Src.create "git.index.decoder" ~doc:"logs git's index decoder event"
    include (val Logs.src_log src : Logs.LOG)
  end

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
     input is not empty. Otherwise, it asks __only one time__ to
     refill the input to the user. If the user refill again with an
     empty input, [peek_byte] decides to return [None] (and probably
     terminate the decoding).

     This semantic could fail if we are in this situation:

     -> decode input
     -> wait more input when k = peek_byte
     -> refill by empty input (but the input __is not__ completely
     consumed)
     -> a next refill can returns a filled input or an empty input
     again

     at this stage, the decoder does not care about the last refill
     and just finalize the decoding.

     In reality, this is does not appear when we read __a file__.
     [read ic == 0] means the end of the file. But can appear on a
     socket ([read socket == 0] means we don't have __yet__ input).
     So, this decoder can not be use on a [socket], only on a file.

     Fortunately, the index data is a file in reality. *)
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
       @@ fun nsec src t -> k { Entry.lsb32; nsec; } src t)
        src t

    let get_kind k src t =
      (get_lsb32
       @@ fun mode src t -> match Int32.((mode && 0xF000l) >> 12), Int32.(mode && 0x1FFl) with
       | 0b1000l, 0o755l -> k Entry.Exec src t
       | 0b0000l, 0o755l -> k Entry.Exec src t
       | 0b1000l, 0o644l -> k Entry.Normal src t
       | 0b0000l, 0o644l -> k Entry.Normal src t
       | 0b1010l, 0o000l -> k Entry.Symlink src t
       | 0b1110l, 0o000l -> k Entry.Gitlink src t
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
         k { Entry.assume = to_bool assume
           ; extend = Some (to_bool extend)
           ; stage
           ; length } src t)
        src t

    let get_extend k src t =
      (get_lsb16
       @@ fun extend src t -> match extend land 0x8000, extend land 0x4000, extend land 0x2000 with
       | reserved, skip_worktree, intent_to_add ->
         k { Entry.reserved = to_bool reserved
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

    let get_digit ~ext k rest src t =
      let buf = Buffer.create 10 in
      let peek_byte = peek_byte ~ext in

      let rec go rest src t =
        (peek_byte
         @@ fun byte rest src t -> match Option.map Char.unsafe_chr byte with
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

    let get_number ~ext k rest src t =
      let peek_byte = peek_byte ~ext in
      let get_byte = get_byte ~ext in
      let get_digit = get_digit ~ext in

      (peek_byte @@ fun byte -> match Option.map Char.unsafe_chr byte with
        | Some '-' -> (get_byte @@ fun _ -> get_digit @@ fun n rest src t -> k (- n) rest src t)
        | Some ('0' .. '9') -> get_digit k
        | _ -> assert false) (* XXX(dinosaure): TODO. *)
      rest src t

    let get_octal ~ext k rest src t =
      let buf = Buffer.create 10 in
      let peek_byte = peek_byte ~ext in

      let rec go rest src t =
        (peek_byte
         @@ fun byte rest src t -> match Option.map Char.unsafe_chr byte with
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
      let get_number = get_number ~ext:(fun k -> Tree k)
      let get_hash = get_hash ~ext:(fun k -> Tree k)

      let get_entry k rest src t =
        (to_nul
         @@ fun path -> get_number
         @@ fun covered -> space @@ get_digit
         @@ fun subtree -> lf
         @@ fun rest src t ->
         match path, covered with
         | "", -1 -> k `Invalid rest src t
         | _ ->
           get_hash (fun hash rest src t ->
                k (`Entry { Ext.path = Fpath.v path
                          ; covered
                          ; subtree
                          ; hash })
                  rest src t)
             rest src t)
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
           split on [Fpath.dir_sep], so it's better to use
           [String.concat] instead [Fpath.append]. *)
    in

    match previous_name with
    | Some previous_name ->
      (KEntry.get_varint
       @@ fun len' ->
       let lead = String.sub previous_name 0 (String.length previous_name - len') in
       KEntry.to_nul
       @@ fun rest _ t ->
       let entry = { Entry.info; hash; flag; path = Fpath.v (lead ^ rest) } in
       Cont { t with entries = entry :: t.entries
                   ; state = Padding 1 })
        src t
    | None ->
      if flag.length = 0xFFF
      then (KEntry.to_nul
            @@ fun name _ t ->
            let entry = { Entry.info
                        ; hash
                        ; flag = { flag with Entry.length = String.length name; }
                        ; path = Fpath.v name } in
            Cont { t with entries = entry :: t.entries
                        ; state = Padding 1 })
          src t
      else (KEntry.take flag.length
            @@ fun name _ t ->
            let entry = { Entry.info; hash; flag; path = Fpath.v name } in
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
       { Entry.ctime
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
        KEntry.get_extend (fun extend -> path info hash { flag with Entry.extend = Some extend })
      | Some false | None ->
        path info hash { flag with Entry.extend = (None : Entry.extend option) }))
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

      let ext =
        if List.exists ((=) `Invalid) entries
        then `Invalid
        else `Entries (List.map (function `Entry x -> x | _ -> assert false) entries)
      in
      Cont { t with state = Signature signature
                  ; extensions = (Ext.Cached_tree ext) :: t.extensions }
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

module MakeIndexEncoder (H: S.HASH) = struct

  module Hash = H
  module Entry = Entry(H)

  type error

  let pp_error = Fmt.nop

  type t =
    { o_off : int
    ; o_pos : int
    ; o_len : int
    ; write : int
    ; version : int32
    ; prev  : Fpath.t option
    ; entries : Entry.index
    ; hash : Hash.Digest.ctx
    ; state : state }
  and state =
    | Header of k
    | Entry of k
    | Hash of k
    | Exception of error
    | End of Hash.t
  and k = Cstruct.t -> t -> res
  and res =
    | Error of t * error
    | Cont  of t
    | Flush of t
    | Ok    of t * Hash.t

  let pp_state ppf = function
    | Header _ -> Fmt.pf ppf "(Header #k)"
    | Exception err -> Fmt.pf ppf "(Exception %a)" pp_error err
    | End hash -> Fmt.pf ppf "(End %a)" Hash.pp hash
    | Entry _ -> Fmt.string ppf "(Entry #k)"
    | Hash _ -> Fmt.string ppf "(Hash #k)"

  let ok t hash : res = Ok ({ t with state = End hash }, hash)
  let flush dst t : res =
    Hash.Digest.feed t.hash (Cstruct.sub dst t.o_off t.o_pos);
    Flush t
  let error t exn : res = Error ({ t with state = Exception exn }, exn)

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>o_off = %d;@ \
                         o_pos = %d;@ \
                         o_len = %d;@ \
                         write = %d;@ \
                         entries = %a;@ \
                         state = %a;@] }"
      t.o_off t.o_pos t.o_len
      t.write
      (Fmt.Dump.list Entry.pp_entry) t.entries
      pp_state t.state

  let rec put_byte ~ctor byte (k : k) dst t =
    if (t.o_len - t.o_pos) > 0
    then begin
      Cstruct.set_uint8 dst (t.o_off + t.o_pos) byte;

      k dst { t with o_pos = t.o_pos + 1
                    ; write = t.write + 1 }
    end else flush dst { t with state = ctor (fun dst t -> (put_byte[@tailcall]) ~ctor byte k dst t) }

  module Int32 =
  struct
    include Int32

    let ( >> ) = Int32.shift_right
    let ( && ) = Int32.logand
    let ( ! )  = Int32.to_int
  end

  let rec put_lsb32 ~ctor integer k dst t =
    if (t.o_len - t.o_pos) >= 4
    then begin
      Cstruct.BE.set_uint32 dst (t.o_off + t.o_pos) integer;
      k dst { t with o_pos = t.o_pos + 4
                   ; write = t.write + 4 }
    end else if (t.o_len - t.o_pos) > 0
    then
      let i1 = Int32.(! ((integer && 0xFF000000l) >> 24))[@warning "-44"] in
      let i2 = Int32.(! ((integer && 0x00FF0000l) >> 16))[@warning "-44"] in
      let i3 = Int32.(! ((integer && 0x0000FF00l) >> 8))[@warning "-44"] in
      let i4 = Int32.(! (integer && 0x000000FFl))[@warning "-44"] in

      (put_byte ~ctor i1
       @@ put_byte ~ctor i2
       @@ put_byte ~ctor i3
       @@ put_byte ~ctor i4 k)
        dst t
    else flush dst { t with state = ctor (fun dst t -> (put_lsb32[@tailcall]) ~ctor integer k dst t) }

  let rec put_lsb16 ~ctor integer k dst t =
    if (t.o_len - t.o_pos) >= 2
    then begin
      Cstruct.BE.set_uint16 dst (t.o_off + t.o_pos) integer;
      k dst { t with o_pos = t.o_pos + 2
                   ; write = t.write + 2 }
    end else if (t.o_len - t.o_pos) > 0
    then
      let i1 = (integer land 0xFF00) lsr 8 in
      let i2 = (integer land 0xFF) in

      (put_byte ~ctor i1
       @@ put_byte ~ctor i2 k)
        dst t
    else flush dst { t with state = ctor (fun dst t -> (put_lsb16[@tailcall]) ~ctor integer k dst t) }

  let flush_without_digest _ t = Flush t

  let put_hash ?(flush = flush) ~ctor hash (k : k) dst t =
    if t.o_len - t.o_pos >= Hash.Digest.length
    then begin
      Cstruct.blit_from_string hash 0 dst (t.o_off + t.o_pos) Hash.Digest.length;
      k dst { t with o_pos = t.o_pos + Hash.Digest.length
                   ; write = t.write + Hash.Digest.length }
    end else
      let rec loop rest dst t =
        if rest = 0
        then k dst t
        else
          let n = min rest (t.o_len - t.o_pos) in

          if n = 0
          then flush dst { t with state = ctor (loop rest) }
          else begin
            Cstruct.blit_from_string hash (Hash.Digest.length - rest) dst (t.o_off + t.o_pos) n;
            flush dst { t with state = ctor (loop (rest - n))
                             ; o_pos = t.o_pos + n
                             ; write = t.write + n }
          end
      in

      loop Hash.Digest.length dst t

  module KHash =
  struct
    let put_hash hash k dst t =
      put_hash ~flush:flush_without_digest ~ctor:(fun k -> Hash k) hash k dst t
  end

  module KHeader =
  struct
    let put_byte byte =
      put_byte ~ctor:(fun k -> Header k) byte
    let put_lsb32 n =
      put_lsb32 ~ctor:(fun k -> Header k) n
  end

  module KEntry =
  struct
    let put_byte byte k dst t =
      put_byte ~ctor:(fun k -> Entry k) byte k dst t

    let put_lsb32 n k dst t =
      put_lsb32 ~ctor:(fun k -> Entry k) n k dst t

    let put_lsb16 n k dst t =
      put_lsb16 ~ctor:(fun k -> Entry k) n k dst t

    let put_time { Entry.lsb32; nsec; } k dst t =
      (put_lsb32 lsb32
       @@ put_lsb32 nsec k)
      dst t

    let put_hash hash k dst t =
      put_hash ~ctor:(fun k -> Entry k) hash k dst t

    let put_kind kind k dst t =
      let open Entry in
      let lsb32 = match kind with
      | Exec    -> Int32.((0b1000l << 12) || 0o755l)
      | Normal  -> Int32.((0b1000l << 12) || 0o644l)
      | Symlink -> Int32.((0b1010l << 12) || 0o000l)
      | Gitlink -> Int32.((0b1110l << 12) || 0o000l) in
      put_lsb32 lsb32 k dst t

    let put_flag flag k dst t =
      let open Entry in
      let lsb16 =
        (if flag.assume then 0x8000 else 0x0)
        lor (if Option.is_some flag.extend then 0x4000 else 0x0)
        lor (flag.stage lsl 12)
        lor (flag.length land 0x0FFF) in
      put_lsb16 lsb16 k dst t

    let put_extend extend k dst t =
      let open Entry in
      let lsb16 =
        (if extend.reserved then 0x8000 else 0x0)
        lor (if extend.skip_worktree then 0x4000 else 0x0)
        lor (if extend.intent_to_add then 0x2000 else 0x0) in
      put_lsb16 lsb16 k dst t

    let tmp_varint = Bytes.create 10

    let put_varint len k dst t =
      let pos = ref 9 in
      let vla = ref len in

      while !vla lsr 7 <> 0
      do
        vla := !vla lsr 7;
        pos := !pos - 1;
        Bytes.set tmp_varint !pos (Char.chr (128 lor ((!vla - 1) land 127)));
        vla := !vla - 1;
      done;

      let rec loop idx dst t =
        if idx = 10
        then k dst t
        else
          let byte = Char.code (Bytes.get tmp_varint idx) in
          put_byte byte (loop (idx + 1)) dst t
      in

      loop !pos dst t

    let nop k dst t = k dst t

    let put_string str k dst t =
      let rec go pos dst t =
        if pos = String.length str
        then put_byte 0 k dst t
        else put_byte (Char.code (String.get str pos)) (go (pos + 1)) dst t
      in go 0 dst t

    let put_bounded_string str k dst t =
      let rec go pos dst t =
        if pos = String.length str
        then k dst t
        else put_byte (Char.code (String.get str pos)) (go (pos + 1)) dst t
      in go 0 dst t
  end

  let trunk value previous =
    let pos = ref 0 in
    let sat chr =
      let ret =
        try Char.equal chr (String.get previous !pos)
        with _ -> false in
      incr pos;
      ret in
    Astring.String.span ~sat value

  let rec padding k dst t =
    if (t.write - 4) mod 8 = 0
    then k dst t
    else KEntry.put_byte 0 (padding k) dst t

  let path = fun x k dst t ->
    if t.version = 4l
    then match t.prev with
      | Some prev ->
        Log.debug (fun l -> l "Encode path %a with the previous path %a."
                        Fpath.pp x.Entry.path Fpath.pp prev);

        let prev = Fpath.to_string prev in
        let path = Fpath.to_string x.Entry.path in
        let (common, value) = trunk path prev in
        let cut = String.length prev - String.length common in

        (KEntry.put_varint cut @@ KEntry.put_string value (if t.version = 4l then k else padding k)) dst t

      | None ->
        (KEntry.put_varint 0 @@ KEntry.put_string (Fpath.to_string x.Entry.path) k) dst t
    else
      KEntry.put_string (Fpath.to_string x.Entry.path) (padding k) dst t

  let hash dst t =
    let hash = Hash.Digest.get t.hash in
    KHash.put_hash (Hash.to_string hash) (fun _ t -> ok t hash) dst t

  let rec switch ?current dst t =
    match t.entries with
    | [] -> flush dst { t with state = Hash hash }
    (* XXX(dinosaure): force to flush and digest all. *)
    | x :: r ->
      Cont { t with state = Entry (entry x)
                  ; entries = r
                  ; prev = Option.map (fun x -> x.Entry.path) current }

  and entry x dst t =
    let open Entry in

    (   KEntry.put_time  x.info.ctime
     @@ KEntry.put_time  x.info.mtime
     @@ KEntry.put_lsb32 x.info.dev
     @@ KEntry.put_lsb32 x.info.ino
     @@ KEntry.put_kind  x.info.mode
     @@ KEntry.put_lsb32 x.info.uid
     @@ KEntry.put_lsb32 x.info.gid
     @@ KEntry.put_lsb32 x.info.size
     @@ KEntry.put_hash  (Hash.to_string x.hash)
     @@ KEntry.put_flag  x.flag
     @@ (match x.flag.extend with
         | Some extend ->
           KEntry.put_extend extend
         | None -> KEntry.nop)
     @@ path x (switch ~current:x))
      (* XXX(dinosaure): no padding in the version 4 of the INDEX file. *)
      dst t

  let many dst t =
    KHeader.put_lsb32 (Int32.of_int (List.length t.entries))
      (fun _ t ->
         Cont { t with state = Entry switch })
      dst t

  let version dst t =
    KHeader.put_lsb32 t.version many dst t

  let header dst t =
    (   KHeader.put_byte (Char.code 'D')
     @@ KHeader.put_byte (Char.code 'I')
     @@ KHeader.put_byte (Char.code 'R')
     @@ KHeader.put_byte (Char.code 'C') version)
      dst t

  let default entries =
    { o_off = 0
    ; o_pos = 0
    ; o_len = 0
    ; write = 0
    ; version = 2l
    ; prev  = None
    ; entries = List.sort (fun a b -> Fpath.compare a.Entry.path b.Entry.path) entries
    ; hash  = Hash.Digest.init ()
    ; state = Header header }

  let eval dst t =
    let eval0 t =
      match t.state with
      | Header k -> k dst t
      | Entry k -> k dst t
      | Hash k -> k dst t
      | Exception err -> Error (t, err)
      | End hash -> Ok (t, hash)
    in

    let rec loop t = match eval0 t with
      | Cont t -> loop t
      | Error (t, err) -> `Error (t, err)
      | Ok (t, hash) -> `End (t, hash)
      | Flush t -> `Flush t
    in

    loop t

  let used_out { o_pos; _ } = o_pos
  let flush off len t =
    { t with o_off = off
           ; o_len = len
           ; o_pos = 0 }
end

module IO (H: S.HASH) (FS: S.FS) = struct

  module Hash = H
  module FileSystem = FS
  module IndexDecoder
    : module type of MakeIndexDecoder(H)
      with module Hash = H
       and module Entry = Entry(H)
    = MakeIndexDecoder(H)
  module IndexEncoder
    : module type of MakeIndexEncoder(H)
      with module Hash = H
       and module Entry = Entry(H)
    = MakeIndexEncoder(H)

  module Log = struct
    let src = Logs.Src.create "git.index" ~doc:"logs git's index event"
    include (val Logs.src_log src : Logs.LOG)
  end

  type error =
    [ `IndexDecoder of IndexDecoder.error
    | `IO of string
    | `File of FileSystem.File.error ]

  let pp_error ppf = function
    | `IndexDecoder err -> Fmt.pf ppf "(`IndexDecoder %a)" IndexDecoder.pp_error err
    | `File err -> Fmt.pf ppf "(`File %a)" FileSystem.File.pp_error err
    | `IO err -> Fmt.pf ppf "(`IO %s)" err

  let load ~root ~dtmp =
    let open Lwt.Infix in

    FileSystem.File.open_r ~mode:0o400 Fpath.(root / "index")[@warning "-44"]
    >>= function
    | Error sys_err ->
      Log.debug (fun l -> l "Retrieve a file-system error: %a." FileSystem.File.pp_error sys_err);
      Lwt.return (Error (`File sys_err))
    | Ok read ->
      let decoder = IndexDecoder.default in

      let rec loop decoder = match IndexDecoder.eval dtmp decoder with
        | `Error (_, err) -> Lwt.return (Error (`IndexDecoder err))
        | `End (_, index, extensions) -> Lwt.return (Ok (index, extensions))
        | `Await decoder ->
          FileSystem.File.read dtmp read >>= function
          | Error sys_err -> Lwt.return (Error (`File sys_err))
          | Ok n ->
            Log.debug (fun l -> l "Reading %d byte(s) of the file-descriptor" n);
            loop (IndexDecoder.refill 0 n decoder)
      in

      loop decoder

  let store ~root ~raw entries =
    let open Lwt.Infix in

    let module E = struct
      type state  = IndexEncoder.t
      type raw    = Cstruct.t
      type result = Hash.t
      type error  = [ `Never ]

      let raw_length = Cstruct.len
      let raw_blit   = Cstruct.blit

      type rest =
        [ `End of (state * result)
        | `Flush of state ]

      let eval raw state =
        match IndexEncoder.eval raw state with
        | #rest as rest -> Lwt.return rest
        | `Error (t, _) -> Lwt.return (`Error (t, `Never))

      let used = IndexEncoder.used_out
      let flush = IndexEncoder.flush
    end in
    FileSystem.File.open_w ~mode:0o644 Fpath.(root / "index")[@warning "-44"]
    >>= function
    | Error sys_err ->
      Lwt.return (Error (`File sys_err))
    | Ok oc ->
      let state = IndexEncoder.default entries in

      Lwt.finalize (fun () ->
          Helper.safe_encoder_to_file
            ~limit:50 (module E) FileSystem.File.write oc raw state)
        (fun () ->
           FileSystem.File.close oc >>= function
           | Ok () -> Lwt.return ()
           | Error sys_err ->
             Log.err (fun l ->
                 l "Got an error while closing %a: %a."
                   Fpath.pp Fpath.(root / "index")
                   FileSystem.File.pp_error sys_err);
             Lwt.return ())
        >|= function
        | Ok hash ->
          Log.debug (fun l -> l "Saved index file with the hash: %a." Hash.pp hash);
          Ok ()
        | Error err ->
          match err with
          | `Stack -> Error (`IO (Fmt.strf "Impossible to store the index file."))
          | `Writer err -> Error (`File err)
          | `Encoder `Never -> assert false
end

module Container (S: Store.S) = struct

  module Store = S
  module StringMap = Map.Make(String)
  module IO
    : module type of IO(Store.Hash)(Store.FS)
      with module Hash = Store.Hash
       and module FileSystem = Store.FS
       and module IndexDecoder = MakeIndexDecoder(Store.Hash)
       and module IndexDecoder.Entry = Entry(Store.Hash)
    = IO(Store.Hash)(Store.FS)
  module Entry = IO.IndexDecoder.Entry

  type 'entry elt =
    | Blob of 'entry
    | Tree of 'entry elt StringMap.t
  and 'entry t = Root of 'entry elt StringMap.t

  let compare_elt a b = match a, b with
    | Tree _, Blob _ -> (-1)
    | Blob _, Tree _ -> 1
    | Tree _, Tree _ -> 0
    | Blob a, Blob b -> Fpath.compare a.Entry.path b.Entry.path

  let pp_blob ?(last = false) ppf (name, entry) =
    Fmt.pf ppf "%s %a"
      (if last then "`-" else "|-")
      (Fmt.hvbox (Fmt.Dump.pair Fmt.string Entry.pp_entry)) (name, entry);
    if not last then Fmt.pf ppf "@\n"

  let rec pp_aux ppf = function
    | [] -> ()
    | [ name, Blob entry ] ->
      pp_blob ~last:true ppf (name, entry)
    | [ name, Tree sub ] ->
      pp_tree ~last:true ppf (name, sub)
    | (name, Blob entry) :: rest ->
      pp_blob ppf (name, entry);
      pp_aux ppf rest
    | (name, Tree sub) :: rest ->
      pp_tree ppf (name, sub);
      pp_aux ppf rest

  and pp_tree ?(last = false) ppf (name, map) =
    Fmt.pf ppf "%s @[<2>%s@\n%a@]"
      (if last then "`-" else "|-")
      name pp_aux (List.sort (fun (_, a) (_, b) -> compare_elt a b) (StringMap.bindings map));
    if not last then Fmt.pf ppf "@\n"

  let pp ppf (Root map) =
    Fmt.pf ppf ".@\n%a\n%!"
      pp_aux (List.sort (fun (_, a) (_, b) -> compare_elt a b) (StringMap.bindings map))

  let chain_of_entry path entry =
    let segs = Fpath.segs path in
    List.fold_right
      (fun seg t -> Tree (StringMap.singleton seg t))
      segs
      (Blob entry)
    |> function Blob _ -> raise (Invalid_argument "Path is empty")
              | Tree map -> map

  let rec merge ~b2b ~e2e _ a b = match a, b with
    | Some x, None -> Some x
    | None, Some x -> Some x
    | Some (Blob a), Some (Blob b) -> b2b a b
    | Some (Tree a), Some (Tree b) -> Some (Tree (StringMap.merge (merge ~b2b ~e2e) a b))
    | Some a, Some b -> e2e a b
    | None, None -> None

  let b2b_new a b =
    let open Entry in

    if a.info.mtime > b.info.mtime
    then Some (Blob a)
    else Some (Blob b)

  let b2b_fix a b =
    if a = b (* XXX(dinosaure): make an equal function. *)
    then Some (Blob a)
    else None

  let old_to_new _ b = Some b
  let new_to_old a _ = Some a
  let fix_to_fix _ _ = None

  let add_new entry (Root t) =
    let chain = chain_of_entry entry.Entry.path entry in
    Root (StringMap.merge (merge ~b2b:b2b_new ~e2e:old_to_new) t chain)

  let add_fix entry (Root t) =
    let chain = chain_of_entry entry.Entry.path entry in
    Root (StringMap.merge (merge ~b2b:b2b_fix ~e2e:fix_to_fix) t chain)

  let update = add_new
  let add = add_fix

  let of_entries entries =
    List.fold_left (fun t x -> add x t) (Root StringMap.empty) entries

  let lwt_result_traversal ~root acc fblob ftree (Root map) =
    let queue = Queue.create () in
    let open Lwt.Infix in
    let ( >?= ) = Lwt_result.bind in

    Lwt_list.iter_s
      (fun (name, value) -> Lwt.return (Queue.add (Fpath.(root / name), value) queue))
      (StringMap.bindings map)
    >>= fun () ->

    let rec go acc = match Queue.take queue with
      | path, Blob entry ->
        fblob acc path entry >?= fun acc -> go acc
      | path, Tree sub ->
        ftree path >?= fun () ->
        Lwt_list.iter_s
          (fun (name, v) -> Lwt.return (Queue.add (Fpath.(path / name), v) queue))
          (StringMap.bindings sub) >>= fun () -> go acc
      | exception Queue.Empty ->
        Lwt.return (Ok acc)
    in go acc

  type error =
    [ `File of Store.FS.File.error
    | `IO of string
    | `Stack
    | `Directory of Store.FS.Dir.error
    | `Store of Store.error ]

  let pp_error ppf = function
    | `File err -> Fmt.pf ppf "(`File %a)" Store.FS.File.pp_error err
    | `Directory err -> Fmt.pf ppf "(`Directory %a)" Store.FS.Dir.pp_error err
    | `IO err -> Fmt.pf ppf "(`IO %s)" err
    | `Stack -> Fmt.pf ppf "Unable to perform I/O operation."
    | `Store err -> Fmt.pf ppf "(`Store %a)" Store.pp_error err

  exception Close of Store.FS.File.error

  open Lwt.Infix
  let ( >?= ) = Lwt_result.bind
  let ( >!= ) = Lwt_result.bind_lwt_err

  type file = [ `Everybody | `Exec | `Normal ]
  type link = [ `Commit | `Link ]
  type directory = [ `Dir ]

  let perm_of_file = function
    | `Everybody -> 0o664
    | `Normal -> 0o644
    | `Exec -> 0o755

  let pp_stats ppf stats =
    let open Unix in

    let pp_kind ppf = function
      | S_REG -> Fmt.string ppf "REG"
      | S_DIR -> Fmt.string ppf "DIR"
      | S_CHR -> Fmt.string ppf "CHR"
      | S_BLK -> Fmt.string ppf "BLK"
      | S_LNK -> Fmt.string ppf "LNK"
      | S_FIFO -> Fmt.string ppf "FIFO"
      | S_SOCK -> Fmt.string ppf "SOCK"
    in

    let pp_octal ppf n =
      let rec loop k = function
        | 0 -> k ()
        | n ->
          loop (fun () -> Fmt.int ppf (n mod 8); k ()) (n / 8)
      in
      loop (fun () -> ()) n
    in

    Fmt.pf ppf "{ @[<hov>dev = %d;@ \
                ino = %d;@ \
                kind = %a;@ \
                perm = %a;@ \
                nlink = %d;@ \
                uid = %d;@ \
                gid = %d;@ \
                rdev = %d;@ \
                size = %d;@ \
                atime = %f;@ \
                mtime = %f;@ \
                ctime = %f;@] }"
      stats.st_dev
      stats.st_ino
      pp_kind stats.st_kind
      pp_octal stats.st_perm
      stats.st_nlink
      stats.st_uid
      stats.st_gid
      stats.st_rdev
      stats.st_size
      stats.st_atime
      stats.st_mtime
      stats.st_ctime

  let entry_of_stats
      hash path
      ?(gitlink = false)
      ({ Unix.st_dev
       ; st_ino
       ; st_kind
       ; st_perm
       ; st_uid
       ; st_gid
       ; st_size
       ; st_mtime
       ; st_ctime
       ; _ } as stats) =
    let info =
      let open Entry in
      { ctime = { lsb32 = Int32.of_float st_ctime
                ; nsec  = Int32.of_float st_ctime }
      ; mtime = { lsb32 = Int32.of_float st_mtime
                ; nsec  = Int32.of_float st_mtime }
      ; dev   = Int32.of_int st_dev
      ; ino   = Int32.of_int st_ino
      ; mode  =
          if gitlink then Gitlink
          else (match st_kind, st_perm with
              | Unix.S_REG, 0o755 -> Exec
              | Unix.S_REG, 0o644 -> Normal
              | Unix.S_LNK, _ -> Symlink
              | _, _ -> raise (Invalid_argument
                                 (Fmt.strf "Unable to make a new entry for \
                                            this File-system object: %a."
                                    pp_stats stats)))
      ; uid = Int32.of_int st_uid
      ; gid = Int32.of_int st_gid
      ; size = Int32.of_int st_size } in
    let flag =
      let open Entry in
      let length =
        let n = String.length (Fpath.to_string path) in
        min 0xFFF n in
      { assume = true; extend = None; stage = 0; length; } in
    { Entry.info; hash; flag; path; }

  let write_blob acc path ((perm : Store.Value.Tree.perm), raw) =
    let file_err err = Lwt.return (`File err) in

    match perm with
    | #link -> raise (Failure "Cannot create symlink/gitlink yet.")
    | #file as file ->
      Store.FS.File.open_w ~mode:(perm_of_file file) path >!= file_err >?= fun fd ->
        let rec loop ~retry raw =
          if Cstruct.len raw = 0
          then Lwt.return (Ok ())
          else Store.FS.File.write raw fd >!= file_err >?= function
              | 0 when retry >= 50 -> Lwt.return (Error `Stack)
              | 0 -> loop ~retry:(retry + 1) raw
              | len ->
                loop ~retry:0 (Cstruct.shift raw len)
        in
        Lwt.catch
          (fun () ->
             Lwt.finalize
               (fun () -> loop ~retry:0 (raw :> Cstruct.t))
               (fun result ->
                  Log.debug (fun l -> l "Blob %a:%a wrote."
                                Store.Hash.pp Store.Value.Blob.(digest (of_cstruct raw))
                                Fpath.pp path);

                  Store.FS.File.close fd >>= function
                  | Ok () -> Lwt.return result
                  | Error err -> Lwt.fail (Close err)))
          (function Close err -> Lwt.return (Error (`File err))
                  | exn -> Lwt.fail exn)
        >?= fun () ->
          Unix.stat (Fpath.to_string path) |> fun stats ->
          Lwt.return (Ok (entry_of_stats Store.Value.Blob.(digest (of_cstruct raw)) path stats :: acc))
    | #directory ->
      raise (Invalid_argument "Unable to make a new directory from a \
                               Git blob object.")

  let write_entry git acc path (entry : Entry.entry) =
    let store_err err = Lwt.return (`Store err) in

    let to_tree_perm x : Store.Value.Tree.perm =
      let open Entry in
      match x with
      | Normal -> `Normal
      | Exec -> `Exec
      | Symlink -> `Link
      | Gitlink -> `Commit
    in

    Store.read git entry.Entry.hash >!= store_err >?= function
      | Store.Value.Blob raw ->
        write_blob acc path (to_tree_perm entry.Entry.info.mode, (raw :> Cstruct.t))
      | _ ->
        Lwt.return (Error `Expected_blob)

  let write_tree dir =
    Store.FS.Dir.create ~path:true dir
    >!= (fun err -> Lwt.return (`Directory err))
    >?= (fun _ -> Lwt.return (Ok ()))

  let from_index git ~dtmp =
    IO.load ~root:(Store.dotgit git) ~dtmp >?= fun (entries, _) ->
    lwt_result_traversal
      ~root:(Store.root git)
      []
      (write_entry git)
      write_tree
      (of_entries entries)

  let from_entries git ~dtmp:raw entries =
    IO.store ~root:(Store.dotgit git) ~raw entries

  let chain_of_path path =
    let segs = Fpath.segs path in
    List.fold_right
      (fun seg t -> Tree (StringMap.singleton seg t))
      segs
      (Tree StringMap.empty)
    |> function Blob _ -> assert false
              | Tree map -> map

  let from_tree git hash =
    let b2b _ _ = raise (Invalid_argument "Try to merge 2 blobs to the same path.") in
    let e2e _ _ = raise (Invalid_argument "Try to merge 2 differents objects to the same path.") in

    let hashtbl = Hashtbl.create 64 in

    Store.fold git
      (fun (Root t) ?name ~length:_ _ value ->
         match name, value with
         | Some path, Store.Value.Blob blob ->
           (try
             let perm = Hashtbl.find hashtbl path in
             let chain = chain_of_entry path (perm, (blob :> Cstruct.t)) in
             Lwt.return (Root (StringMap.merge (merge ~b2b ~e2e) t chain))
            with Not_found -> Lwt.return (Root t))
         | Some path, Store.Value.Tree tree ->
           List.iter
             (fun { Store.Value.Tree.perm; name; _ } ->
                Hashtbl.add hashtbl Fpath.(path / name) perm)
             (Store.Value.Tree.to_list tree);

           let chain = chain_of_path path in
           Lwt.return (Root (StringMap.merge (merge ~b2b ~e2e) t chain))
         | _ -> Lwt.return (Root t))
      ~path:(Fpath.v "/") (Root StringMap.empty) hash >>= fun root ->
    lwt_result_traversal
      ~root:(Store.root git)
      []
      write_blob
      write_tree
      root

  let from_commit git hash =
    (Store.read git hash >!= fun err -> Lwt.return (`Store err)) >?= function
    | Store.Value.Commit commit ->
      from_tree git (Store.Value.Commit.tree commit)
    | _ -> Lwt.fail (Invalid_argument "Expected a Git commit object.")

  let from_reference git reference =
    Store.Ref.list git >>= fun lst ->
    let hash = List.assoc reference lst in
    from_commit git hash
end
