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

(** Implementation of a Git stores.

    This implementation is more complete than the memory back-end because
    firstly, Git was think to be use on a file-system. Then, because for each
    operations, we let the client to control the memory consumption.

    So we provide a more powerful API which let the user to notice aready
    allocated buffers outside this scope and process some I/O operations on
    pools. *)

module type Rs = sig
  type +'a fiber

  type t

  type error

  val pp_error : error Fmt.t

  val atomic_wr : t -> Reference.t -> string -> (unit, error) result fiber

  (* open / single_write / close *)
  val atomic_rd : t -> Reference.t -> (string, error) result fiber

  (* open / single_read / close *)
  val atomic_rm : t -> Reference.t -> (unit, error) result fiber

  (* unlink *)
  val list : t -> Reference.t list fiber

  (* readdir / closedir *)

  type fd

  val input_line : fd -> string option fiber
end

module type Mj = sig
  include Carton_git.STORE

  include
    Smart_git.APPEND
      with type t := t
       and type uid := uid
       and type fd := fd
       and type error := error
       and type +'a fiber := 'a fiber
end

open Reference

module Make
    (Digestif : Digestif.S)
    (Mn : Loose_git.STORE
            with type +'a fiber = 'a Lwt.t
             and type uid = Digestif.t)
    (Mj : Mj with type +'a fiber = 'a Lwt.t)
    (Rs : Rs with type +'a fiber = 'a Lwt.t) : sig
  type t

  val v :
    minor:Mn.t ->
    major:Mj.t ->
    idx:(Mj.uid -> Mj.uid) ->
    ?packed:Digestif.t Packed.packed ->
    refs:Rs.t ->
    Fpath.t ->
    t Lwt.t

  include
    Minimal.S
      with type hash = Digestif.t
       and type index = Mj.uid
       and type pack = Mj.uid
       and type t := t
end
