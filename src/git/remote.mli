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

(** Store-less operations on a remote Git repository.

    The functions of this module do not require the user to keep a local store.
    They allocate an ephemeral {!Mem.Store.t}, exchange the strict minimum with
    the remote repository and forget everything when they return. They exist
    for the {i unikernel} which does not really need an internal store but
    which mostly wants to save something remotely.

    {b Note.} Everything stays in memory. One call costs, at least, the size of
    the snapshot of the remote branch: a {i depth 1} fetch downloads the whole
    tree of the tip, not only its commit. This module is not intended for large
    repositories nor for large files. *)

val push_new_file :
  ?branch:Reference.t ->
  ?author:User.t ->
  ?message:string ->
  ctx:Mimic.ctx ->
  Smart_git.Endpoint.t ->
  path:Fpath.t ->
  contents:string ->
  (unit, [> `Conflict | `Msg of string ]) result Lwt.t
(** [push_new_file ~ctx edn ~path ~contents] adds one new file [path], whose
    contents is [contents], into the remote Git repository [edn] - and only
    that file. It is the store-less equivalent of:

    {[
      $ git clone --depth 1 <edn> repo && cd repo
      $ mkdir -p $(dirname <path>) && cat > <path>
      $ git add <path> && git commit && git push
    ]}

    [path] must be relative and must not contain a ["."] or a [".."] segment.
    The {i mode} of the new file is [`Normal].

    [branch] (default: {!Reference.master}) is the reference updated remotely.
    It must be a full reference name such as {!Reference.master} or
    {!Reference.main}. If [branch] does not exist remotely - in particular if
    the remote repository is empty - it is created, and the new commit is a
    root commit.

    [author] is used as the author {b and} as the committer of the new commit.
    It defaults to an anonymous user dated from the {i epoch}: the [git]
    library does not depend on a clock, so a caller which cares about dates
    must give an [author] such as:

    {[
      let author () =
        let ptime = Ptime.unsafe_of_d_ps (Pclock.now_d_ps ()) in
        {
          Git.User.name = "Romain Calascibetta";
          email = "romain.calascibetta@gmail.com";
          date = (Int64.of_float (Ptime.to_float_s ptime), None);
        }
    ]}

    [message] is the commit message and defaults to ["Add <path>"].

    The result is [Error `Conflict] if [path], or one of its parent
    directories, already exists in the fetched tip - whether as a file or as a
    directory. This function only ever {i adds}: it never replaces nor deletes
    an existing object. Any other error (an invalid [path], the transport, the
    store) is returned as [`Msg].

    {b Note.} The conflict is checked against the tip fetched at the beginning
    of the call. If the remote repository moves in the meantime, the remote
    peer rejects our update. However, {!Sync.S.push} currently only {i logs}
    such a rejection, so [Ok ()] means that the PACK file was sent, not that
    the remote reference was updated. *)
