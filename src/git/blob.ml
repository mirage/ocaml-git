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

module type S =
sig
  type t = private Cstruct.t

  module Hash
    : Ihash.S
  module D
    : S.DECODER with type t = t
                 and type raw = Cstruct.t
                 and type init = Cstruct.t
                 and type error = [ `Decoder of string ]
  module E
    : S.ENCODER with type t = t
                      and type raw = Cstruct.t
  module A
    : S.ANGSTROM with type t = t
  module F
    : S.FARADAY  with type t = t

  include Ihash.DIGEST with type t := t
                        and type hash := Hash.t
  include S.BASE with type t := t
end

module Make
    (H : Ihash.S with type Digest.buffer = Cstruct.t)
  : S with module Hash = H
= struct
  module Hash = H

  type t = Cstruct.t

  module A =
  struct
    type nonrec t = t

    let decoder =
      let buf = Buffer.create 32 in
      let open Angstrom in

      fix @@ fun m ->
      available >>= function
      | 0 ->
        peek_char
        >>= (function
            | Some _ -> commit *> m
            | None ->
              let cs = Cstruct.of_string (Buffer.contents buf) in
              Buffer.clear buf;
              return cs <* commit)
      | n -> take n >>= fun chunk ->
        Buffer.add_string buf chunk;
        commit *> m
  end

  module F =
  struct
    type nonrec t = t

    let length : t -> int64 = fun t ->
      Int64.of_int (Cstruct.len t)

    let encoder : t Farfadet.t = fun e t ->
      Farfadet.bigstring e (Cstruct.to_bigarray t)
  end

  module D =
  struct
    (* XXX(dinosaure): may be need to compare the performance between this
       module and [Helper.MakeDecoder(A)]. *)

    type nonrec t = t
    type init = Cstruct.t

    type error = [ `Decoder of string ]

    let pp_error ppf (`Decoder err) =
      Helper.ppe ~name:"`Decoder" Fmt.string ppf err

    type decoder = { res : t
                   ; cur : Cstruct.t
                   ; abs : int
                   ; final : bool }
    and raw = Cstruct.t

    let default raw =
      { res = raw
      ; cur = Cstruct.sub raw 0 0
      ; abs = 0
      ; final = false }

    let ensure decoder =
      let available = Cstruct.len decoder.res - decoder.abs in

      if available < Cstruct.len decoder.cur
      then begin
        let size = ref (Cstruct.len decoder.res) in
        while !size - decoder.abs < Cstruct.len decoder.cur
        do size := (3 * !size) / 2 done;

        let res' = Cstruct.create !size in
        Cstruct.blit decoder.res 0 res' 0 decoder.abs;
        { decoder with res = res' }
      end else decoder

    let eval decoder =
      if decoder.final
      then `End (decoder.cur, (decoder.res : t))
      else begin
        let decoder = ensure decoder in
        Cstruct.blit decoder.cur 0 decoder.res decoder.abs (Cstruct.len decoder.cur);
        `Await { decoder with abs = decoder.abs + Cstruct.len decoder.cur }
      end

    let refill input decoder =
      Ok { decoder with cur = input }

    let finish decoder =
      let res' = Cstruct.sub decoder.res 0 decoder.abs in
      { decoder with final = true
                   ; res = res' }

    let to_result input = Ok input
  end

  module E =
  struct
    type nonrec t = t
    type init = t

    type error = [ `Encoder of string ]

    let pp_error ppf (`Encoder err) =
      Helper.ppe ~name:"`Encoder" Fmt.string ppf err

    type encoder =
      { abs  : int
      ; off  : int
      ; pos  : int
      ; len  : int
      ; blob : t }
    and raw = Cstruct.t

    let default blob =
      { abs = 0
      ; off = 0
      ; pos = 0
      ; len = 0
      ; blob }

    let eval raw encoder =
      if Cstruct.len encoder.blob = encoder.abs
      then `End (encoder, Cstruct.len encoder.blob)
      else begin
        let n = min (Cstruct.len encoder.blob - encoder.abs) encoder.len in

        Cstruct.blit encoder.blob encoder.abs raw encoder.off encoder.len;

        if encoder.abs + n = Cstruct.len encoder.blob
        then `End ({ encoder with abs = encoder.abs + n
                                ; pos = encoder.pos + n },
                   Cstruct.len encoder.blob )
        else `Flush ({ encoder with abs = encoder.abs + n
                                  ; pos = encoder.pos + n })
      end

    let used encoder = encoder.pos

    let flush off len encoder =
      { encoder with off = off
                   ; len = len
                   ; pos = 0 }

    let to_result raw = Ok raw
  end

  let digest cs =
    let ctx = Hash.Digest.init () in
    let hdr = Fmt.strf "blob %Ld\000" (F.length cs) in

    Hash.Digest.feed ctx (Cstruct.of_string hdr);
    Hash.Digest.feed ctx cs;
    Hash.Digest.get ctx

  let pp_cstruct ppf cs =
    for i = 0 to Cstruct.len cs - 1
    do match Cstruct.get_char cs i with
      | '\000' .. '\031' | '\127' -> Fmt.const Fmt.char '.' ppf ()
      | chr -> Fmt.char ppf chr
    done

  let pp      = pp_cstruct
  let equal   = Cstruct.equal
  let compare = Cstruct.compare
  let hash    = Hashtbl.hash

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end
