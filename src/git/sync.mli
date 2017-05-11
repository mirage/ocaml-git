(*
 * Copyright (c) 2013-2017 Thomas Gazagnaire <thomas@gazagnaire.org>
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

type protocol = [ `SSH | `Git | `Smart_HTTP ]

val protocol: Uri.t -> [`Ok of protocol | `Not_supported of string | `Unknown]

type capability =
  [ `Multi_ack
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

val pp_capability: capability Fmt.t

module Result: sig

  type fetch

  val head_contents: fetch -> Reference.head_contents option
  val head: fetch -> Hash.Commit.t option
  val references: fetch -> Hash.t Reference.Map.t
  val hashes: fetch -> Hash.Set.t
  val pp_fetch: fetch Fmt.t
  type ok_or_error = [`Ok | `Error of string]

  type push = {
    result  : ok_or_error;
    commands: (Reference.t * ok_or_error) list;
  }

  val pp_push: push Fmt.t

end

type want = [ `Ref of Reference.t | `Commit of Hash.Commit.t ]

val pp_want: want Fmt.t

module type S = sig
  type t
  type ctx
  val ls: ?ctx:ctx -> t -> Gri.t -> Hash.t Reference.Map.t Lwt.t
  val push: ?ctx:ctx -> t -> branch:Reference.t -> Gri.t -> Result.push Lwt.t
  val fetch:
    ?ctx:ctx ->
    ?deepen:int ->
    ?unpack:bool ->
    ?capabilities:capability list ->
    ?wants:want list ->
    ?progress:(string -> unit) ->
    t -> Gri.t -> Result.fetch Lwt.t
  val clone:
    ?ctx:ctx ->
    ?deepen:int ->
    ?unpack:bool ->
    ?capabilities:capability list ->
    ?branch:want ->
    ?progress:(string -> unit) ->
    t -> checkout:bool -> Gri.t -> Result.fetch Lwt.t
end

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

module Make (IO: IO) (S: Store.S): S with type t = S.t and type ctx = IO.ctx
