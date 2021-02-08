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

(** The synchronization commands to a git repository. *)

(** [Sync] interface used by backend-specific Git implementations
    such as [Mem] and [Git_unix] *)
module type S = sig
  type hash
  type store
  type error = private [> Mimic.error | `Exn of exn ]

  val pp_error : error Fmt.t

  val fetch :
    ?push_stdout:(string -> unit) ->
    ?push_stderr:(string -> unit) ->
    ctx:Mimic.ctx ->
    Smart_git.Endpoint.t ->
    store ->
    ?version:[> `V1 | `V2 ] ->
    ?capabilities:Smart.Capability.t list ->
    ?deepen:[ `Depth of int | `Timestamp of int64 ] ->
    [ `All | `Some of (Reference.t * Reference.t) list | `None ] ->
    ((hash * (Reference.t * hash) list) option, error) result Lwt.t

  val push :
    ctx:Mimic.ctx ->
    Smart_git.Endpoint.t ->
    store ->
    ?version:[> `V1 | `V2 ] ->
    ?capabilities:Smart.Capability.t list ->
    [ `Create of Reference.t
    | `Delete of Reference.t
    | `Update of Reference.t * Reference.t ]
    list ->
    (unit, error) result Lwt.t
end

(** Creates a lower-level [Sync] functions [fetch] and [push] that are then
        overridden by backend-specific implementations such as [Mem] and [Git_unix] *)
module Make
    (Digestif : Digestif.S)
    (Pack : Smart_git.APPEND with type +'a fiber = 'a Lwt.t)
    (Index : Smart_git.APPEND with type +'a fiber = 'a Lwt.t)
    (Store : Minimal.S with type hash = Digestif.t)
    (HTTP : Smart_git.HTTP) : sig
  type hash = Digestif.t
  type store = Store.t
  type error = [ `Exn of exn | `Store of Store.error | Mimic.error ]

  val pp_error : error Fmt.t

  val fetch :
    ?push_stdout:(string -> unit) ->
    ?push_stderr:(string -> unit) ->
    ctx:Mimic.ctx ->
    Smart_git.Endpoint.t ->
    store ->
    ?version:[> `V1 | `V2 ] ->
    ?capabilities:Smart.Capability.t list ->
    ?deepen:[ `Depth of int | `Timestamp of int64 ] ->
    [ `All
    | `Some of
      (* src (remote) ref * dst (local) ref *)
      (Reference.t * Reference.t) list
    | `None ] ->
    src:Pack.uid ->
    dst:Pack.uid ->
    idx:Index.uid ->
    create_idx_stream:(unit -> unit -> string option Lwt.t) ->
    create_pack_stream:(unit -> unit -> string option Lwt.t) ->
    Pack.t ->
    Index.t ->
    ((hash * (Reference.t * hash) list) option, ([> error ] as 'err)) result
    Lwt.t
  (** fetches remote references and saves them.
          Behavior of fetch when [want] is
          [`All] - fetches all remote references and saves them in store
          [`Some src_dst_pairs] - fetch [src] and save in [dst]
          [`None] - doesn't save anything *)

  val push :
    ctx:Mimic.ctx ->
    Smart_git.Endpoint.t ->
    store ->
    ?version:[> `V1 | `V2 ] ->
    ?capabilities:Smart.Capability.t list ->
    [ `Create of Reference.t
    | `Delete of Reference.t
    | `Update of Reference.t * Reference.t ]
    list ->
    (unit, ([> error ] as 'err)) result Lwt.t
end
