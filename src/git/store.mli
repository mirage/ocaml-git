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

(** Implementation of a Git repository with a file-system back-end.

    This implementation is more complete than the memory back-end
    because firstly, Git was think to be use on a file-system. Then,
    because for each operations, we let the client to control the
    memory consumption.

    So we provide a more powerful API which let the user to notice
    aready allocated buffers outside this scope and process some I/O
    operations on pools. *)

module type LOOSE = sig

  type t
  (** The type of the {i loosed} Git object. *)

  type state
  (** The type of the Git state. *)

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]
  (** Kind of the {i loosed} Git object. *)

  module Hash: S.HASH
  (** The [Hash] module used to make the implementation. *)

  module FS: S.FS
  (** The [FS] module used to make the implementation. *)

  module Inflate: S.INFLATE
  (** The [Inflate] module used to make the implementation. *)

  module Deflate: S.DEFLATE
  (** The [Deflate] module used to make the implementation. *)

  module Value: Value.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)
     and type t = Value.Make(Hash)(Inflate)(Deflate).t
  (** The Value module, which represents the Git object. *)

  type error =
    [ `FS of FS.error
    | `IO of string
    | Value.D.error
    | Value.E.error
    ] (** The type error. *)

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  val lookup_p: state -> Hash.t -> Hash.t option Lwt.t
  (** [lookup_p state hash] try to find the object associated by the
      hash [hash] in the file-system. This function can be use in the
      concurrency context. This function returns [None] if the Git
      object does not exist or is not stored as a {i loose} object. *)

  val lookup: state -> Hash.t -> Hash.t option Lwt.t
  (** An alias of {!lookup_p}. *)

  val mem: state -> Hash.t -> bool Lwt.t
  (** [mem state hash] checks if one object satisfies the predicate
      [digest(object) = hash]. This function is the same as [lookup
      state hash <> None]. *)

  val list: state -> Hash.t list Lwt.t
  (** [list state] lists all available git {i loose} objects in the
      file-system. The list returned does not contain all git objects
      available in your repository but only which {i loosed} one. *)

  val read_p :
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> state -> Hash.t -> (t, error) result Lwt.t
  (** [read_p ~ztmp ~dtmp ~raw ~window state hash] can retrieve a git
      {i loose} object from the file-system. It de-serializes the git
      object to an OCaml value. This function needs some buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated
      flow (and keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the
      specified buffers are not used by another process. This function
      does not allocate any buffer and uses only the specified buffers
      to construct the OCaml value. The git object returned respects
      the predicate [digest(result) = hash]. Otherwise, we return an
      {!error}:

      {ul
      {- {!FS.File.error} when we can not access to the git
      object in the file-system (because it does not exist or the
      structure of the git repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the
      requested git {i loose} object. That means, the git {i loose}
      object does not respect the Git format.}} *)

  val read_s: state -> Hash.t -> (t, error) result Lwt.t
  (** [read_s state hash] is the same process than {!read_p} but we
      use the state-defined buffer. That means the client can not use
      this function in a concurrency context with the same [state]. *)

  val read: state -> Hash.t -> (t, error) result Lwt.t
  (** Alias of {!read_s}. *)

  val size_p :
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> state -> Hash.t -> (int64, error) result Lwt.t
  (** [size_p ~ztmp ~dtmp ~raw ~window state hash] returns the size of
      the git {i loose} object which respects the predicate
      [digest(object) = hash]. The size is how many byte(s) are needed
      to store the serialized (but not inflated) git object in bytes
      (without the header).

      As {!read_p}, {!size_p} needs some buffers. The client can use
      this function in a concurrency context when the buffers
      specified are not used by another process.

      As {!read_p}, {!size_p} can return an {!error}. *)

  val size_s: state -> Hash.t -> (int64, error) result Lwt.t
  (** [size_s state hash] is the same process than {!size_p} but we
      use the state-defined buffer. That means the client can not use
      this function in a concurrency context with the same [state]. *)

  val size: state -> Hash.t -> (int64, error) result Lwt.t
  (** Alias of {!size_s}. *)

  val write_p:
       ztmp:Cstruct.t
    -> raw:Cstruct.t
    -> state -> t -> (Hash.t * int, error) result Lwt.t
  (** [write_p ~ztmp ~raw state v] writes as a {i loose} git object
      the value [v] in the file-system. It serializes and deflates the
      value to a new file, which respects the internal structure of a
      git repository. This function needs some buffers:

      {ul
      {- [ztmp] is a buffer used to store the deflated flow}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the
      specified buffers are not used by another process. This function
      does not allocate any buffer and uses only the specified buffers
      to store the OCaml value to the file-system. Then, the OCaml
      value will be available by [read state digest(v)]. Otherwise, we
      return an {!error}:

      {ul
      {- {!FS.File.error} when we can not create a new file in
      the file-system.}
      {- {!Value.E.error} when we can not serialize xor deflate the
      requested git {i loose} object. This kind of error should be
      never happen.}}

      The current implementation does not limit the memory consumption
      of the deflate computation (i.e. {i zlib} and the flush method).
      Depending of the object [v], the process can consume a lot of
      memory. *)

  val write_s: state -> t -> (Hash.t * int, error) result Lwt.t
  (** [write_s state v] is the same process than {!write_p} but we use
      the state-defined buffer. That means the client can not use this
      function in a concurrency context with the same [state]. *)

  val write: state -> t -> (Hash.t * int, error) result Lwt.t
  (** Alias of {!write_s}. *)

  val write_inflated: state -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  (** [write_inflated state kind raw] writes the git object in the git
      repository [state] and associates the kind to this object. This
      function does not verify if the raw data is well-defined (and
      respects the Git format). Then, this function returns the hash
      produced from the kind and the inflated raw to let the user to
      retrieve it.

      If we retrieve any error error while the I/O operations, we {b
      raise} 9by {!Lwt.fail} a [Failure] which describe in a [string]
      the error encountered. *)

  val raw_p:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> window:Inflate.window
    -> raw:Cstruct.t
    -> state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  (** [raw_p ~window ~ztmp ~dtmp ~raw state hash] can retrieve a git
      {i loose} object from the file-system. However, this function {b
      does not} de-serialize the git object and returns the kind of
      the object and the {i raw-data} inflated. Precisely, it decodes
      only the header. This function needs some buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated
      flow (and keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the
      specified buffers are not used by another process. This function
      allocate only one buffer in the major-heap and uses only the
      specified buffers to compute the {i raw-data} in the allocated
      {!Cstruct.t}. The {i raw-data} respects the predicate
      [digest(header + raw-data) = hash]. Otherwise, we return an
      {!error}:

      {ul
      {- {!FS.File.error} when we can not access to the git
      object in the file-system (because it does not exist or the
      structure of the git repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the
      requested git {i loose} object. That means, the git {i loose}
      object has a wrong header or it is corrupted.}} *)

  val raw_s: state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  (** [raw_s state hash] is the same process than {!raw_p} but we use
      the state-defined buffer. That means the client can not use this
      function in a concurrency context with the same [state]. *)

  val raw: state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  (** Alias of {!raw_s}. *)

  val raw_wa:
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> window:Inflate.window
    -> raw:Cstruct.t
    -> result:Cstruct.t
    -> state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  (** [raw_wa ~window ~ztmp ~dtmp ~raw ~result state hash] can
      retrieve a git {i loose} object from the file-system. However,
      this function {b does not} de-serialize the git object and
      returns the kind of the object and the {i raw-data} inflated.
      Precisely, it decodes only the header. This function needs some
      buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated
      flow (and keep it for an alteration)}
      {- [raw] is a buffer used to the input flow}
      {- [result] is a buffer to store the result of this process}}

      The suffix [wa] refers to: Without Allocation. Indeed, this
      function allocates {b only} any data in the minor-heap. All
      other needed OCaml objects must be noticed by the client.
      However, the client needs to notice a well sized [result]
      (otherwise, the client can retrieve an error). He can get this
      information by {!size}.

      This function can be used in a concurrency context only if the
      specified buffers are not used by another process. The {i
      raw-data} respects the predicate [digest(header + raw-data) =
      hash]. Otherwise, we return an {!error}:

      {ul
      {- {!FS.File.error} when we can not access to the git
      object in the file-system (because it does not exist or the
      structure of the git repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the
      requested git {i loose} object. That means, the git {i loose}
      object has a wrong header or it is corrupted.}}

      In a server context, this function should be used to limit the
      allocation. *)

  val raw_was: Cstruct.t ->
    state -> Hash.t -> (kind * Cstruct.t, error) result Lwt.t
  (** [raw_was result state hash] is the same process than {!raw_wa}
      but we use the state-defined buffer. That means the client can
      not use this function in a concurrency context with the same
      [state]. *)

  module D: S.DECODER
    with type t = t
     and type init = Inflate.window * Cstruct.t * Cstruct.t
     and type error = Value.D.error
  (** The decoder of the Git object. We constraint the input to be an
      {!Inflate.window} and a {Cstruct.t} which used by the {Inflate}
      module and an other {Cstruct.t} as an internal buffer.

      All error from the {!Inflate} module is relayed to the
      [`Inflate] error value.

      The decoder includes an header process. This decoder does not
      correspond directly to a de-serialized Git object but a
      de-serialized Git {i loose} object. *)

  module E: S.ENCODER
    with type t = t
     and type init = int * t * int * Cstruct.t
     and type error = Value.E.error

  (** The encoder (which uses a {!Minienc.encoder}) of the Git object.
      We constraint the output to be a {Cstruct.t}. This encoder needs
      the level of the compression, the value {!t}, the memory
      consumption of the encoder (in bytes - must be a power of two)
      and an internal buffer between the compression and the encoder.

      All error from the {!Deflate} module is relayed to the
      [`Deflate] error value.

      The encoder includes an header process. This encoder does not
      correspond directly to a serialized Git object but a serialized
      Git {i loose} object. That means we put in front of the Git
      object an header:

      > kind length\000 ... *)
end

module type PACK = sig

  type t
  (** The type of the {i packed} Git object. *)

  type state
  (** The type of the Git state. *)

  type value
  (** The type of the Git value. *)

  module Hash: S.HASH
  (** The [Hash] module used to make the implementation. *)

  module FS: S.FS
  (** The [FS] module used to make the implementation. *)

  module Inflate: S.INFLATE
  (** The [Inflate] module used to make the implementation. *)

  module Deflate: S.DEFLATE
  (** The [Deflate] module used to make the implementation. *)

  module PACKDecoder: Unpack.DECODER
    with module Hash = Hash
     and module Inflate = Inflate
  (** The [PACKDecoder] module, which decodes {i PACK} file. *)

  module PACKEncoder: Pack.ENCODER
    with module Hash = Hash
     and module Deflate = Deflate

  module IDXDecoder: Index_pack.LAZY with module Hash = Hash
  (** The [IDXDecoder] module, which decodes {i IDX} file. *)

  module IDXEncoder: Index_pack.ENCODER
    with module Hash = Hash

  module Pack_info: Pack_info.S
    with module Hash = Hash
     and module Inflate = Inflate

  (** The type error. *)
  type error =
    [ `PackDecoder of PACKDecoder.error
    | `PackEncoder of PACKEncoder.error
    | `PackInfo of Pack_info.error
    | `IdxDecoder of IDXDecoder.error
    | `IdxEncoder of IDXEncoder.error
    | `FS of FS.error
    | `Invalid_hash of Hash.t
    | `Delta of PACKEncoder.Delta.error
    | `IO of string
    | `Integrity of string
    | `Not_found ]

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  val lookup: state -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t
  (** [lookup state hash] try to find the object associated by the
      hash [hash] in all {i IDX} files available in the current git
      repository [state]. This function can be used in a concurrency
      context. This function returns [None] if the Git object does not
      exist in any {i IDX} files or it does not exists in the current
      repository. *)

  val mem: state -> Hash.t -> bool Lwt.t
  (** [mem state hash] checks if one object satisfies the predicate
      [digest(object) = hash]. This function is the same as [lookup
      state hash <> None]. *)

  val list: state -> Hash.t list Lwt.t
  (** [list state] lists all {i packed} git object noticed by all {i
      IDX} files available in the current git repository [state]. If
      we encountered any error, we make it silent and continue the
      process. *)

  val read_p :
       ztmp:Cstruct.t
    -> window:Inflate.window
    -> state -> Hash.t -> (t, error) result Lwt.t
  (** [read_p ~ztmp ~window state hash] can retrieve a git {i packed}
      object from any {i PACK} files available in the current git
      repository [state]. It just inflates the git object and informs
      some meta-data (like kind, CRC-32 check-sum, length, etc.) about
      it. Then, the client can use the related decoder to get the
      OCaml value. This function needs some buffers:

      {ul
      {- [window] is a buffer used y the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}}

      This function can be used in a concurrency context only if the
      specified buffers are not used by another process. This function
      allocates 2 {Cstruct.t} needed to re-construct the git {i
      packed} object in any case (if it is delta-ified or not) and a
      certain amount of little buffer (sized by 0x7F bytes) to help
      the construction only when the requested object is delta-ified.
      Otherwise, we return an {!error}:

      {ul
      {- {!FS.File.error} or {!FS.Dir.error} or
      {!FS.Mapper.error} when we retrieve a file-system error}
      {- {!PACKDecoder.error} when we retrieve an error about the
      decoding of the {i packed} git object in the founded {i PACK}i
      file}
      {- {!IDXDecoder.error} when we retrieve an error about the
      decoding of an {i IDX} file}
      {- [`Not_found] when the requested object is not {i packed}}} *)

  val read_s: state -> Hash.t -> (t, error) result Lwt.t
  (** [read_s state hash] is the same process than {!read_p} but we
      use the state-defined buffer. That means the client can not use
      this function in a concurrency context with the same [state]. *)

  val read: state -> Hash.t -> (t, error) result Lwt.t
  (** Alias of {!read_s}. *)

  val size_p :
       ztmp:Cstruct.t
    -> window:Inflate.window
    -> state -> Hash.t -> (int, error) result Lwt.t
  (** [size_p ~ztmp ~window state hash] returns the size of the git {i
      packed} object which respects the predicate [digest(object) =
      hash]. The size is how many byte(s) are needed to store the
      serialized (but not inflated) git object in bytes (without the
      header).

      As {!read_p}, {!size_p} needs some buffers. The client can use
      this function in a concurrency context when the buffers
      specified are not used by another process.

      As {!read_p}, {!size_p} can return an {!error}. *)

  val size_s: state -> Hash.t -> (int, error) result Lwt.t
  (** [size_s state hash] is the same process than {!size_p} but we
      use the state-defined buffer. That means the client can not use
      this function in a concurrency context with the same [state]. *)

  val size: state -> Hash.t -> (int, error) result Lwt.t
  (** Alias of {size_s}. *)

  type stream = unit -> Cstruct.t option Lwt.t
  (** The stream contains the PACK flow. *)

  module Graph: Map.S with type key = Hash.t

  val from: state -> stream -> (Hash.t * int, error) result Lwt.t
  (** [from git stream] populates the Git repository [git] from the
      PACK flow [stream]. If any error is encountered, any Git objects
      of the PACK flow are not added in the Git repository. *)

  val make: state
    -> ?window:[ `Object of int | `Memory of int ]
    -> ?depth:int
    -> value list
    -> (stream * (Crc32.t * int64) Graph.t Lwt_mvar.t, error) result Lwt.t
  (** [make ?window ?depth values] makes a PACK stream from a list of
      {!Value.t}.

      [?window] specifies the weight of the window while the
      delta-ification. The client can limit the weight by the number
      of objects in the windoz (by default, is 10) or by the weight in
      byte of the window in your memory.

      [depth] (default is [50]) limits the depth of the
      delta-ification.

      Then, the function returns a stream and a protected variable
      which contains a representation of the associated IDX file of
      the PACK stream. This protected variable is available
      ([Lwt_mvar.take]) only {b at the end} of the PACK stream. That
      means, the client needs to consume all of the stream and only
      then he can take the [Graph.t] associated. *)
end

module type S =
sig
  type t
  (** The type of the git repository. *)

  module Hash: S.HASH
  (** The [Digest] module used to make the implementation. *)

  module Inflate
   : S.INFLATE
  (** The [Inflate] module used to make the implementation. *)

  module Deflate
   : S.DEFLATE
  (** The [Deflate] module used to make the implementation. *)

  module FS : S.FS
  (** The [FS] module used to make the implementation. *)

  module Value: Value.S
    with module Hash = Hash
     and module Inflate = Inflate
     and module Deflate = Deflate
     and module Blob = Blob.Make(Hash)
     and module Commit = Commit.Make(Hash)
     and module Tree = Tree.Make(Hash)
     and module Tag = Tag.Make(Hash)
     and type t = Value.Make(Hash)(Inflate)(Deflate).t
  (** The Value module, which represents the Git object. *)

  module Reference: Reference.IO
    with module Hash = Hash
     and module FS = FS
  (** The Reference module, which represents the Git reference. *)

  module PACKDecoder: Unpack.DECODER
    with module Hash = Hash
     and module Inflate = Inflate
  (** The [PACKDecoder] module used to decode a {i PACK} file. *)

  module PACKEncoder: Pack.ENCODER
    with module Hash = Hash
     and module Deflate = Deflate
  (** The [PACKEncoder] module used to encoder a {i PACK} file. *)

  module Loose: LOOSE
    with type t = Value.t
     and type state = t
     and module Hash = Hash
     and module FS = FS
     and module Inflate = Inflate
     and module Deflate = Deflate
  (** The [Loose] module which represents any {i loose} git object
      available in git repository. *)

  module Pack: PACK
    with type t = PACKDecoder.Object.t
     and type value = Value.t
     and type state = t
     and module Hash = Hash
     and module FS = FS
     and module Inflate = Inflate
  (** The [Pack] module which represents any {i packed} git object
      available in the git repository. *)

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]
  (** Kind of the {i packed} Git object. *)

  (** The type error. *)
  type error =
    [ Loose.error
    | Pack.error ]

  val pp_error: error Fmt.t
  (** Pretty-printer of {!error}. *)

  val create :
       ?root:Fpath.t
    -> ?dotgit:Fpath.t
    -> ?compression:int
    -> unit -> (t, error) result Lwt.t
  (** [create ?root ?dotgit ?compression ()] creates a new store
      represented by the path [root] (default is ["."]), where the Git
      objects are located in [dotgit] (default is [root / ".git"] and
      when Git objects are compressed by the [level] (default is
      [4]). *)

  val dotgit: t -> Fpath.t
  (** [dotgit state] returns the current [".git"] path used. *)

  val root: t -> Fpath.t
  (** [root state] returns the current path of the repository. *)

  val compression: t -> int
  (** [compression state] returns the current level of the compression
      used to write a git object. *)

  val mem: t -> Hash.t -> bool Lwt.t
  (** [mem state hash] checks if one object of the current repository
      [state] satisfies the predicate [digest(object) = hash]. *)

  val list: t -> Hash.t list Lwt.t
  (** [list state] lists all git objects available in the current
      repository [state]. *)

  val read_p :
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> t -> Hash.t -> (Value.t, error) result Lwt.t
  (** [read_p ~ztmp ~dtmp ~raw ~window state hash] can retrieve a git
      object from the current repository [state]. It de-serializes the
      git object to an OCaml value. This function needs some buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated
      flow (and keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the
      specified buffers are not used by another process. This function
      follows the same scheme of allocation of {!Loose.read_p} if the
      requested git object is a {i loose} git object or {!Pack.read_p}
      if the requested git object is a {i packed} git object.
      Otherwise, we return an {!error}. *)

  val read_s: t -> Hash.t -> (Value.t, error) result Lwt.t
  (** [read_s state hash] is the same process than {!read_p} but we
      use the state-defined buffer. That means the client can not use
      this function in a concurrency context with the same [state]. *)

  val read: t -> Hash.t -> (Value.t, error) result Lwt.t
  (** Alias of {!read_s}. *)

  val read_exn: t -> Hash.t -> Value.t Lwt.t
  (** [read_exn state hash] is an alias of {!read} but raise an
      exception (instead to return a {!result}) if the git object
      requested does not exist or if we catch any other errors. *)

  val write_p :
       ztmp:Cstruct.t
    -> raw:Cstruct.t
    -> t -> Value.t -> (Hash.t * int, error) result Lwt.t
  (** [write_p ~ztmp ~raw state v] writes as a {b loose} git object
      [v] in the file-system. It serializes and deflates the value to
      a new file. which respect the internal structure of a git
      repository. This function needs some buffers:

      {ul
      {- [ztmp] is a buffer used to store the deflated flow}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the
      specified buffers are not used by another process. This function
      does not allocate any buffer and uses only the specified buffers
      to store the OCaml value to the file-system. Then, the OCaml
      value will be available by [read state digest(v)]. Otherwise, we
      return an {!error}:

      {ul
      {- {!FS.File.error} when we can not create a new file in
      the file-system.}
      {- {!Value.E.error} when we can not serialize xor deflate the
      requested git {i loose} object. This kind of error should be
      never happen.}} *)

  val write_s: t -> Value.t -> (Hash.t * int, error) result Lwt.t
  (** [write_s state v] is the process than {!write_p} but we use the
      state-defined buffer. That means the client can not use this
      function in a concurrency context with te same [state]. *)

  val write: t -> Value.t -> (Hash.t * int, error) result Lwt.t
  (** Alias of {!write_s}. *)

  val size_p :
    ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> t -> Hash.t -> (int64, error) result Lwt.t
  (** [size_p ~ztmp ~dtmp ~raw ~window state hash] returns the size of
      the git object which respects the predicate [digest(object) =
      hash]. The size is how many byte(s) are needed to store the
      serialized (but not inflated) git object in bytes (without the
      header).

      As {!read_p}, {!size_p} needs some buffers. The client can use
      this function in a concurrency context when the buffers
      specified are not used by another process.

      As {!read_p}, {!size_p} can return an {!error}. *)

  val size_s: t -> Hash.t -> (int64, error) result Lwt.t
  (** [size_s state hash] is the same process than {!size_p} but we
      use the state-defined buffer. That means the client can not use
      this function in a concurrency context with the same [state]. *)

  val size: t -> Hash.t -> (int64, error) result Lwt.t
  (** Alias of {!size_s}. *)

  val raw_p :
       ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  (** [raw_p ~window ~ztmp ~dtmp ~raw state hash] can retrieve a git
      object available in the current git repository [state]. However,
      this function {b does not} de-serialize the git object and
      returns the kind of the object and the {i raw-data} inflated.
      Precisely, it decodes only the header. This function needs some
      buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated
      flow (and keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the
      specified buffers are not used by another process. This function
      follows the same scheme of allocation of {!Loose.raw_p} if the
      requested git object is a {i loose} git object or {!Pack.read_p}
      if the requested git object is a {i packed} git object.
      Otherwise, we return an {!error}. *)

  val raw_s: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  (** [raw_s state hash] is the same process than {!raw_p} but we use
      the state-defined buffer. That means the client can not use this
      function in a concurrency context with the same [state]. *)

  val raw: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  (** Alias of {!raw_s}. *)

  val read_inflated: t -> Hash.t -> (kind * Cstruct.t) option Lwt.t
  (** Alias of {!raw_s}. *)

  val write_inflated: t -> kind:kind -> Cstruct.t -> Hash.t Lwt.t
  (** [write_inflated state kind raw] writes the git object in the git
      repository [state] and associates the kind to this object. This
      function does not verify if the raw data is well-defined (and
      respects the Git format). Then, this function returns the hash
      produced from the kind and the inflated raw to let the user to
      retrieve it.

      If we retrieve any error error while the I/O operations, we {b
      raise} 9by {!Lwt.fail} a [Failure] which describe in a [string]
      the error encountered. *)

  val contents: t -> ((Hash.t * Value.t) list, error) result Lwt.t
  (** [contents state] returns an associated list between the hash and
      its bind git object. This list contains all git objects
      available in the current git repository [state]. *)

  val buffer_window: t -> Inflate.window
  (** [buffer_window state] returns the state-defined [window]. *)

  val buffer_zl: t -> Cstruct.t
  (** [buffer_zl state] returns the state-defined buffer used to store
      any inflated flow. *)

  val buffer_de: t -> Cstruct.t
  (** [buffer_de state] returns the state-defined buffer used as
      internal buffer for any decoder. *)

  val buffer_io: t -> Cstruct.t
  (** [buffer_io state] returns the state-defined buffer used to store
      the input flow. *)

  val fold:
       t
    -> ('a -> ?name:Fpath.t -> length:int64 -> Hash.t -> Value.t -> 'a Lwt.t)
    -> path:Fpath.t
    -> 'a
    -> Hash.t
    -> 'a Lwt.t
  (** [fold state f ~path acc hash] iters on any git objects reachable
      by the git object [hash] which located in [path] (for example,
      if you iter on a commit, [path] should be ["."] - however, if
      you iter on a tree, [path] should be the directory path
      represented by your tree). For each git objects, we notice the
      path [name] (derived from [path]) if the object is a Blob or a
      Tree, the [length] or the git object (see {!size}), the [hash]
      and the [value].

      If the [hash] points to:

      {ul
      {- {!Value.Blob.t}: [f] is called only one time with the OCaml
      value of the {i blob}.}
      {- {!Value.Tree.t}: [f] is called firstly with the Ocaml value
      of the pointed {i tree} by the hash [hash]. Then, we {i iter}
      (and call [f] for each iteration) in the list of entries of the
      {i tree}. Finally, we retrieve recursively all sub-tree objects
      and do an ascending walk. [f] is never called more than one time
      for each hash.}
      {- {!Value.Commit.t}: [f] is called firstly with the OCaml value
      of the pointed {i commit} by the hash [hash]. Then, it follozs
      recursively all parents of the current commit, Finallym it
      starts a [fold] inside the pointed root {i tree} git object of
      each {i commit} previously retrieved. [f] never called more than
      one time for each hash.}
      {- {!Value.Tag.t}: [f] is called firstly with the OCaml value of
      the pointed {i tag} by the hash [hash]. Then, it follows the git
      object pointed by the {i tag}.}}

      Any retrieved {!error} is missed. *)

  module Ref :
  sig
    module Packed_refs: Packed_refs.S
      with module Hash = Hash
       and module FS = FS

    type nonrec error =
      [ Packed_refs.error
      | error
      | `Invalid_reference of Reference.t
      ] (** The type error. *)

    val pp_error: error Fmt.t
    (** Pretty-printer of {!error}. *)

    val mem: t -> Reference.t -> bool Lwt.t
    (** [mem state ref] returns [true] iff [ref] exists in [state],
        otherwise returns [false]. *)

    val graph_p :
      dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> t -> (Hash.t Reference.Map.t, error) result Lwt.t
    (** [graph_p state ~dtmp ~raw] returns a graph which contains all
        reference and its pointed to final hash available in the
        current git repository [state]. This function needs some
        buffers:

        {ul
        {- [dtmp] is a buffer used by the decoder to save the input
        flow (and keep it for an alteration)}
        {- [raw] is a buffer used to store the input flow}}

        This function can be used in a concurrency context only if the
        specified buffers are not used by another process. If we
        encountered any {!error}, we make it silent and continue the
        process. *)

    val graph: t -> (Hash.t Reference.Map.t, error) result Lwt.t
    (** [graph state] is the same process than {!graph_p} but we use
        the state-defined buffer. That means the client can not use
        this function in a concurrency context with the same
        [state]. *)

    val normalize: Hash.t Reference.Map.t -> Reference.head_contents -> (Hash.t, error) result Lwt.t
    (** [normalize graph ref] returns the final hash pointed by the
        reference [ref]. This function can return an error:

        {ul
        {- [`Invalid_reference ref] if, from the graph [graph], we
        don't find the final pointed hash. This case can appear if the
        graph is not complete or if the link is broken.}} *)

    val list_p :
      dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> t -> (Reference.t * Hash.t) list Lwt.t
    (** [list_p state ~dtmp ~raw] returns an associated list between
        reference and its bind hash. This function is the same than:

        > graph state >|= Reference.Map.fold (fun x acc -> x :: acc)

        As {!graph_p}, this function needs some buffers. And, as
        {!graph_p}, this function can be used in a concurrency context
        only f the specified buffers are not used by another process.
        Finally, as {!graph_p}, if we encountered any {!error}, we
        make it silent and continue the process. *)

    val list_s: t -> (Reference.t * Hash.t) list Lwt.t
    (** [list_s state] is the same process than {!list_p} but we use
        the state-defined buffer. That means the client can not use
        this function in a concurrency context with the same
        [state]. *)

    val list: t -> (Reference.t * Hash.t) list Lwt.t
    (** Alias of {!list_s}. *)

    val remove_p :
      dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> t -> Reference.t -> (unit, error) result Lwt.t
    (** [remove_p state ~dtmp ~raw reference] removes the reference
        [reference] from the git repository [state]. This function
        needs some buffers:

        {ul
        {- [dtmp] is a buffer used by the decoder to save the input
        flow (and keep it for an alteration)}
        {- [raw] is a buffer used to store the input flow}}

        This function can {b not} used in a concurrency context.

        However, we provide this function to allow to use a
        user-defined buffer instead the state-defined buffer (could be
        used by something else). *)

    val remove_s: t -> Reference.t -> (unit, error) result Lwt.t
    (** [remove_s state reference] is the same process than
        {!remove_p} but we use the state-defined buffer. That means
        the client can not use thus function in a concurrency context
        with the same [state]. *)

    val remove: t -> Reference.t -> (unit, error) result Lwt.t
    (** [remove state reference] removes the reference [reference]
        from the git repository [state]. *)

    val read_p :
      dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> t -> Reference.t -> ((Reference.t * Reference.head_contents), error) result Lwt.t
    (** [read_p state ~dtmp ~raw reference] returns the value contains
        in the reference [reference] (available in the git repository
        [state]). This function needs some buffers:

        {ul
        {- [dtmp] is a buffer used by the decoder to save the flow
        (and keep it for an alteration)}
        {- [raw] is a buffer used to store the input flow}}

        This function can be used in a concurrency context only if the
        specifed buffers are not used by another process. *)

    val read_s: t -> Reference.t ->
      ((Reference.t * Reference.head_contents), error) result Lwt.t
    (** [read_s state reference] is the same process than {!read_p}
        but we use the state-defined buffer. That means the client can
        not use this function in a concurrency context with the same
        [state]. *)

    val read: t -> Reference.t ->
      ((Reference.t * Reference.head_contents), error) result Lwt.t
    (** Alias of {!read_s}. *)

    val write_p :
      dtmp:Cstruct.t
      -> raw:Cstruct.t -> t -> Reference.t -> Reference.head_contents ->
      (unit, error) result Lwt.t
    (** [write_p state ~dtmp ~raw reference value] writes the value
        [value] in the mutable representation of the [reference] in
        the git repository [state].

        As {!read_p}, this function needs some buffers. However, it
        can not use in a concurrency context by the mutable
        caracteristic of the reference. In other side, we let the
        client to specify an user-defined buffer instead the
        state-defined buffer of {!write_s} to use in a concurrency
        context an other operation. *)

    val write_s: t -> Reference.t -> Reference.head_contents ->
      (unit, error) result Lwt.t
    (** [write_s state ?locks reference value] is the same process
        than {!read_p} but we use the state-defined buffer. *)

    val write: t -> Reference.t -> Reference.head_contents ->
      (unit, error) result Lwt.t
    (** Alias of {!write_s}. *)

  end

  val clear_caches: t -> unit Lwt.t
  (** [clear_caches t] drops all values stored in the internal caches
      binded with the git repository [t]. *)

  val reset: t -> (unit, [ `Store of error | `Ref of Ref.error ]) result Lwt.t
  (** [reset t] removes all things of the git repository [t] and
      ensures it will be empty. *)

  val has_global_watches: bool
  val has_global_checkout: bool
end

module FS
    (H: S.HASH)
    (FS: S.FS)
    (Inflate: S.INFLATE)
    (Deflate: S.DEFLATE)
  : S with module Hash = H
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module FS = FS
