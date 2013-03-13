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

(** Git data-model *)

open Lib


(** File contents. *)
module type CONTENTS = sig
  type t
  val dump: t -> unit
  val to_string: t -> string
  val of_string: File.Name.t -> string -> t
end

(** Each component shares the same signature, containing a binary ID, an hex
   ID, and a content. *)
module type SIG = sig
  module Id : Abstract.SIG
  module Hex: Abstract.SIG
  module C  : CONTENTS
  type id = Id.t
  type hex = Hex.t
  type contents = C.t
  type t = {
    hex     : hex;
    id      : id;
    contents: contents;
  }
  include CONTENTS with type t := t
end

(** Blob objects. *)
module Blob: SIG

(** Commit objects. *)
module Commit: SIG

(** Tree objects. *)
module Tree: SIG

(** Tag objects. *)
module Tag: SIG

(** An object is either a tag/tree/commit/blob. *)
module Object: SIG
