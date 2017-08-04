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

module type LOOSE =
sig
  type t
  (** The type of the {i loosed} Git object. *)

  type state
  (** The type of the Git state. *)

  module Hash
    : Ihash.S
  (** The [Hash] module used to make this interface. *)

  module Path
    : Path.S
  (** The [Path] module used to make this interface. *)

  module FileSystem
    : Fs.S with type path = Path.t
  (** The [FileSystem] module used to make this interface. *)

  module Inflate
    : S.INFLATE
  (** The [Inflate] module used to make this interface. *)

  module Deflate
    : S.DEFLATE
  (** The [Deflate] module used to make this interface. *)

  module Value
    : Value.S with module Hash = Hash
               and module Inflate = Inflate
               and module Deflate = Deflate
  (** The Value module, which represents the Git object. *)

  type error = [ `SystemFile of FileSystem.File.error
               | `SystemDirectory of FileSystem.Dir.error
               (** Appears when the file-system returns an error. *)
               | Value.D.error
               (** Appears when the decoder of the Git object returns an
                   error. *)
               | Value.E.error
                 (** Appears w hen the encoder of the Git object returns an
                      error. *)
               ] (** The type error. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  val lookup_p : state -> Hash.t -> Hash.t option Lwt.t
  (** [lookup_p state hash] try to find the object associated by the hash [hash]
      in the file-system. This function can be use in the concurrency context.
      This function returns [None] if the Git object does not exist or is not
      stored as a {i loose} object. *)

  val lookup : state -> Hash.t -> Hash.t option Lwt.t
  (** An alias of {!lookup_p}. *)

  val exists : state -> Hash.t -> bool Lwt.t
  (** [exists state hash] checks if one object satisfies the predicate
      [digest(object) = hash]. This function is the same as [lookup state hash
      <> None]. *)

  val list : state -> Hash.t list Lwt.t
  (** [list state] lists all available git {i loose} objects in the file-system.
      The list returned does not contain all git object available in your
      repository but only which {i loosed} one. *)

  val read_p : ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    window:Inflate.window ->
    state -> Hash.t -> (t, error) result Lwt.t
  (** [read_p ~ztmp ~dtmp ~raw ~window state hash] can retrieve a git {i loose}
      object from the file-system. It de-serializes the git object to an OCaml
      value. This function needs some buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated flow (and
      keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the specified
      buffers are not used by another process. This function does not allocate
      any buffer and uses only the specified buffers to construct the OCaml
      value. The git object returned respects the predicate [digest(result) =
      hash]. Otherwise, we return an {!error}:

      {ul
      {- {!FileSystem.File.error} when we can not access to the git object in the
      file-system (because it does not exist or the structure of the git
      repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the requested
      git {i loose} object. That means, the git {i loose} object does not
      respect the Git format.}}
  *)

  val read_s : state -> Hash.t -> (t, error) result Lwt.t
  (** [read_s state hash] is the same process than {!read_p} but we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val read : state -> Hash.t -> (t, error) result Lwt.t
  (** Alias of {!read_s}. *)

  val size_p : ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    window:Inflate.window ->
    state -> Hash.t -> (int64, error) result Lwt.t
  (** [size_p ~ztmp ~dtmp ~raw ~window state hash] returns the size of the git
      {i loose} object which respects the predicate [digest(object) = hash]. The
      size is how many byte(s) are needed to store the serialized (but not
      inflated) git object in bytes (without the header).

      As {!read_p}, {!size_p} needs some buffers. The client can use this
      function in a concurrency context when the buffers specified are not used
      by another process.

      As {!read_p}, {!size_p} can return an {!error}.
  *)

  val size_s : state -> Hash.t -> (int64, error) result Lwt.t
  (** [size_s state hash] is the same process than {!size_p} but we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val size : state -> Hash.t -> (int64, error) result Lwt.t
  (** Alias of {!size_s}. *)

  val write_p : ztmp:Cstruct.t ->
    raw:Cstruct.t ->
    state -> t -> (Hash.t * int, error) result Lwt.t
  (** [write_p ~ztmp ~raw state v] writes as a {i loose} git object the value
      [v] in the file-system. It serializes and deflates the value to a new
      file, which respects the internal structure of a git repository. This
      function needs some buffers:

      {ul
      {- [ztmp] is a buffer used to store the deflated flow}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the specified
      buffers are not used by another process. This function does not allocate
      any buffer and uses only the specified buffers to store the OCaml value to
      the file-system. Then, the OCaml value will be available by [read state
      digest(v)]. Otherwise, we return an {!error}:

      {ul
      {- {!FileSystem.File.error} when we can not create a new file in the file-system.}
      {- {!Value.E.error} when we can not serialize xor deflate the requested
      git {i loose} object. This kind of error should be never happen.}}

      NOTE: the current implementation does not limit the memory consumption of
      the deflate computation (i.e. {i zlib} and the flush method). Depending of
      the object [v], the process can consume a lot of memory. TODO!
  *)

  val write_s : state -> t -> (Hash.t * int, error) result Lwt.t
  (** [write_s state v] is the same process than {!write_p} bu we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val write : state -> t -> (Hash.t * int, error) result Lwt.t
  (** Alias of {!write_s}. *)

  val raw_p :
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  (** [raw_p ~window ~ztmp ~dtmp ~raw state hash] can retrieve a git {i loose}
      object from the file-system. However, this function {b does not}
      de-serialize the git object and returns the kind of the object and the {i
      raw-data} inflated. Precisely, it decodes only the header. This function
      needs some buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated flow (and
      keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the specified
      buffers are not used by another process. This function allocate only one
      buffer in the major-heap and uses only the specified buffers to compute
      the {i raw-data} in the allocated {!Cstruct.t}. The {i raw-data} respects
      the predicate [digest(header + raw-data) = hash]. Otherwise, we return an
      {!error}:

      {ul
      {- {!FileSystem.File.error} when we can not access to the git object in
      the file-system (because it does not exist or the structure of the git
      repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the requested
      git {i loose} object. That means, the git {i loose} object has a wrong
      header or it is corrupted.}}
  *)

  val raw_s : state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  (** [raw_s state hash] is the same process than {!raw_p} but we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val raw : state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  (** Alias of {!raw_s}. *)

  val raw_wa :
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    result:Cstruct.t ->
    state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  (** [raw_wa ~window ~ztmp ~dtmp ~raw ~result state hash] can retrieve a git {i
      loose} object from thefile-system. However, this function {b does not}
      de-serialize the git object and returns the kind of the object and the {i
      raw-data} inflated. Precisely, it decodes only the header. This function needs
      some buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated flow (and
      keep it for an alteration)}
      {- [raw] is a buffer used to the input flow}
      {- [result] is a buffer to store the result of this process}}

      The suffix [wa] refers to: Without Allocation. Indeed, this function
      allocates {b only} any data in the minor-heap. All other needed OCaml
      objects must be noticed by the client. However, the client needs to notice
      a well sized [result] (otherwise, the client can retrieve an error). He
      can get this information by {!size}.

      This function can be used in a concurrency context only if the specified
      buffers are not used by another process. The {i raw-data} respects the
      predicate [digest(header + raw-data) = hash]. Otherwise, we return an
      {!error}:

      {ul
      {- {!FileSystem.File.error} when we can not access to the git object in
      the file-system (because it does not exist or the structure of the git
      repository is wrong).}
      {- {!Value.D.error} when we can not de-serialize xor inflate the requested
      git {i loose} object. That means, the git {i loose} object has a wrong
      header or it is corrupted.}}

      In a server context, this function should be used to limit the allocation.
  *)

  val raw_was : Cstruct.t ->
    state -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t, error) result Lwt.t
  (** [raw_was result state hash] is the same process than {!raw_wa} but we use
      the state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  module D
    : S.DECODER with type t = t
                      and type raw = Cstruct.t
                      and type init = Inflate.window * Cstruct.t * Cstruct.t
                      and type error = Value.D.error
  (** The decoder of the Git object. We constraint the input to be an
      {!Inflate.window} and a {Cstruct.t} which used by the {Inflate} module and
      an other {Cstruct.t} as an internal buffer.

      All error from the {!Inflate} module is relayed to the [`Inflate] error
      value.

      NOTE: The decoder includes an header process. This decoder does not
      correspond directly to a de-serialized Git object but a de-serialized Git {i
      loose} object.
  *)

  module E
    : S.ENCODER with type t = t
                      and type raw = Cstruct.t
                      and type init = int * t * int * Cstruct.t
                      and type error = Value.E.error
  (** The encoder (which uses a {!Minienc.encoder}) of the Git object. We
      constraint the output to be a {Cstruct.t}. This encoder needs the level of
      the compression, the value {!t}, the memory consumption of the encoder (in
      bytes) and an internal buffer between the compression and the encoder.

      All error from the {!Deflate} module is relayed to the [`Deflate] error
      value.

      NOTE: The encoder includes an header process. This encoder does not
      correspond directly to a serialized Git object but a serialized Git {i loose}
      object. That means we put in front of the Git object an header:

      > kind length\000 ...
  *)
end

module type PACK =
sig
  type t
  (** The type of the {i packed} Git object. *)

  type state
  (** The type of the Git state. *)

  module Hash
    : Ihash.S
  (** The [Hash] module used to make this interface. *)

  module Path
    : Path.S
  (** The [Path] module used to make this interface. *)

  module FileSystem
    : Fs.S with type path = Path.t
  (** The [FileSystem] module used to make this interface. *)

  module Inflate
    : S.INFLATE
  (** The [Inflate] module used to make this interface. *)

  module IDXDecoder
    : Index_pack.LAZY
  (** The [IDXDecoder] module, which decodes {i IDX} file. *)

  module PACKDecoder
    : Unpack.DECODER with module Hash = Hash
                      and module Inflate = Inflate
  (** The [PACKDecoder] module, which decodes {i PACK} file. *)

  type error = [ `SystemFile of FileSystem.File.error
               (** Appears when the file-system returns an error when we try to
                   compute a file. *)
               | `SystemDirectory of FileSystem.Dir.error
               (** Appears when the file-system returns an error when we try to
                   compute a directory. *)
               | `SystemMapper of FileSystem.Mapper.error
               (** Appears when the file-system returns an error when we try to
                   [mmap] a file. *)
               | `IndexDecoder of IDXDecoder.error
               (** Appears when the decoder of the {i IDX} file returns an
                   error. *)
               | `PackDecoder of PACKDecoder.error
               (** Appears when the decoder of the {i PACK} file returns an
                   error. *)
               | `Not_found
                 (** Appears when we don't find in any {i IDX} files the requested
                     {i packed} git object. *)
               ] (** The type error. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  val lookup_p : state -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t
  (** [lookup_p state hash] try to find the object associated by the hash [hash]
      in all {i IDX} files available in the current git repository [state]. This
      function can be used in a concurrency context. This function returns
      [None] if the Git object does not exist in any {i IDX} files or it does
      not exists in the current repository. *)

  val lookup : state -> Hash.t -> (Hash.t * (Crc32.t * int64)) option Lwt.t
  (** An alias of {!lookup_p}. *)

  val exists : state -> Hash.t -> bool Lwt.t
  (** [exists state hash] checks if one object satisfies the predicate
      [digest(object) = hash]. This function is the same as [lookup state hash
      <> None]. *)

  val list : state -> Hash.t list Lwt.t
  (** [list state] lists all {i packed} git object noticed by all {i IDX} files
      available in the current git repository [state]. If we encountered any
      error, we make it silent and continue the process. *)

  val read_p : ztmp:Cstruct.t ->
    window:Inflate.window ->
    state -> Hash.t -> (t, error) result Lwt.t
  (** [read_p ~ztmp ~window state hash] can retrieve a git {i packed} object
      from any {i PACK} files available in the current git repository [state].
      It just inflates the git object and informs some meta-data (like kind,
      CRC-32 check-sum, length, etc.) about it. Then, the client can use the
      related decoder to get the OCaml value. This function needs some buffers:

      {ul
      {- [window] is a buffer used y the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}}

      This function can be used in a concurrency context only if the specified
      buffers are not used by another process. This function allocates 2
      {Cstruct.t} needed to re-construct the git {i packed} object in any case
      (if it is delta-ified or not) and a certain amount of little buffer (sized
      by 0x7F bytes) to help the construction only when the requested object is
      delta-ified. Otherwise, we return an {!error}:

      {ul
      {- {!FileSystem.File.error} or {!FileSystem.Dir.error} or
      {!FileSystem.Mapper.error} when we retrieve a file-system error}
      {- {!PACKDecoder.error} when we retrieve an error about the decoding of
      the {i packed} git object in the founded {i PACK}i file}
      {- {!IDXDecoder.error} when we retrieve an error about the decoding of an
      {i IDX} file}
      {- [`Not_found] when the requested object is not {i packed}}}
  *)

  val read_s : state -> Hash.t -> (t, error) result Lwt.t
  (** [read_s state hash] is the same process than {!read_p} but we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val read : state -> Hash.t -> (t, error) result Lwt.t
  (** Alias of {!read_s}. *)

  val read_wa : ?htmp:Cstruct.t array ->
    ztmp:Cstruct.t ->
    window:Inflate.window ->
    result:Cstruct.t * Cstruct.t ->
    state -> Hash.t -> (t, error) result Lwt.t
  (** [read_wa ?htmp ~ztmp ~window state hash] can retrieve a git {i packed}
      object from any {i PACK} files available in the current git repository
      [state]. It just inflates the git object and informs some meta-data (like
      kind, CRC-32 check-sum, length, etc.) about it. Then, the client can use the
      related decoder to get the OCaml value. This function needs some buffers:

      {ul
      {- [window] is a buffer used y the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}}
      {- [result] is a couple of buffers to store the result of this processi in
      one of these buffers}}

      [htmp] is an array of buffer used for the delta-ification. If you know
      where is the Git object (which {i PACK} file) and the maximum depth of the
      PACK file, you can allocate an array, which one store [Insert] hunks for
      each level of the delta-ification instead to allocate (if [htmp = None]).
      In other side, this will raise an error (typically,
      [Index_out_of_bounds]).

      This function can be used in a concurrency context only if the specified
      buffers are not used by another process. This function does not allocate
      any {!Cstruct.t}. The couple of buffers [result] is used to re-construct
      the Git object requested in any depth of the delta-ification. Then, the
      returned value is physically equal to one of these buffers.

      Otherwise, we return an {!error}:

      {ul
      {- {!FileSystem.File.error} or {!FileSystem.Dir.error} or
      {!FileSystem.Mapper.error} when we retrieve a file-system error}
      {- {!PACKDecoder.error} when we retrieve an error about the decoding of
      the {i packed} git object in the founded {i PACK}i file}
      {- {!IDXDecoder.error} when we retrieve an error about the decoding of an
      {i IDX} file}
      {- [`Not_found] when the requested object is not {i packed}}}
  *)

  val read_was : ?htmp:Cstruct.t array
    -> (Cstruct.t * Cstruct.t) -> state -> Hash.t -> (t, error) result Lwt.t
  (** [read_was result state hash] is the same process than {!read_wa} ut we use
      the state-defined buffers. That means the client can not use this function in
      a concurrency context with the same [state].*)

  val size_p : ztmp:Cstruct.t -> window:Inflate.window -> state -> Hash.t -> (int, error) result Lwt.t
  (** [size_p ~ztmp ~window state hash] returns the size of the git {i packed}
      object which respects the predicate [digest(object) = hash]. The size is how
      many byte(s) are needed to store the serialized (but not inflated) git object
      in bytes (without the header).

      As {!read_p}, {!size_p} needs some buffers. The client can use this
      function in a concurrency context when the buffers specified are not used
      by another process.

      As {!read_p}, {!size_p} can return an {!error}.
  *)

  val size_s : state -> Hash.t -> (int, error) result Lwt.t
  (** [size_s state hash] is the same process than {!size_p} but we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val size : state -> Hash.t -> (int, error) result Lwt.t
  (** Alias of {size_s}. *)
end

module type S =
sig
  type t
  (** The type of the git repository. *)

  module Hash
    : Ihash.S
  (** The [Digest] module used to make the module. *)

  module Path
    : Path.S
  (** The [Path] module used to make the module. *)

  module Inflate
    : S.INFLATE
  (** The [Inflate] module used to make the module. *)

  module Deflate
    : S.DEFLATE
  (** The [Deflate] module used to make the module. *)

  module Lock
    : Lock.S
  (** The [Lock] module used to make this interface. *)

  module FileSystem
    : Fs.S with type path = Path.t
            and type File.lock = Lock.t
  (** The [FileSystem] module used to make the module. *)

  module Value
    : Value.S with module Hash = Hash
               and module Inflate = Inflate
               and module Deflate = Deflate
  (** The Value module, which represents the Git object. *)

  module Reference
    : Reference.S with module Hash = Hash
                   and module Path = Path
                   and module Lock = Lock
                   and module FileSystem = FileSystem
  (** The Reference module, which represents the Git reference. *)

  module IDXDecoder
    : Index_pack.LAZY with module Hash = Hash
  (** The [IDXDecoder] module used to decode an {i IDX} file. *)

  module PACKDecoder
    : Unpack.DECODER with module Hash = Hash
                      and module Inflate = Inflate
  (** The [PACKDecoder] module used to decode a {i PACK} file. *)

  module PACKEncoder
    : Pack.ENCODER with module Hash = Hash
                    and module Deflate = Deflate
  (** The [PACKEncoder] module used to encoder a {i PACK} file. *)

  module Loose
    : LOOSE with type t = Value.t
             and type state = t
             and module Hash = Hash
             and module Path = Path
             and module FileSystem = FileSystem
             and module Inflate = Inflate
             and module Deflate = Deflate
  (** The [Loose] module which represents any {i loose} git object available in
      git repository. *)

  module Pack
    : PACK with type t = PACKDecoder.Object.t
            and type state = t
            and module Hash = Hash
            and module Path = Path
            and module FileSystem = FileSystem
            and module Inflate = Inflate
            and module PACKDecoder = PACKDecoder
            and module IDXDecoder = IDXDecoder
  (** The [Pack] module which represents any {i packed} git object available in
      the git repository. *)

  type error = [ Loose.error
                 (** Appears when the {!Loose} module returns an error. *)
               | Pack.error
                 (** Appears when the {!Pack} module returns an error. *)
               ] (** The type error. *)

  val pp_error : error Fmt.t
  (** Pretty-printer of {!error}. *)

  val create : ?root:Path.t ->
    ?dotgit:Path.t ->
    ?compression:int ->
    unit -> (t, error) result Lwt.t

  val dotgit : t -> Path.t
  (** [dotgit state] returns the current [".git"] path used. *)
  val root : t -> Path.t
  (** [root state] returns the current path of the repository. *)
  val compression : t -> int
  (** [compression state] returns the current level of the compression used to
      write a git object. *)

  val exists : t -> Hash.t -> bool Lwt.t
  (** [exists state hash] checks if one object of the current repository [state]
      satisfies the predicate [digest(object) = hash]. *)

  val list : t -> Hash.t list Lwt.t
  (** [list state] lists all git objects available in the current repository
      [state]. *)

  val read_p : ztmp:Cstruct.t
    -> dtmp:Cstruct.t
    -> raw:Cstruct.t
    -> window:Inflate.window
    -> t -> Hash.t -> (Value.t, error) result Lwt.t
  (** [read_p ~ztmp ~dtmp ~raw ~window state hash] can retrieve a git object
      from the current repository [state]. It de-serializes the git object to an
      OCaml value. This function needs some buffers:

      {ul
      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated flow (and
      keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the specified
      buffers are not used by another process. This function follows the same
      scheme of allocation of {!Loose.read_p} if the requested git object is a
      {i loose} git object or {!Pack.read_p} if the requested git object is a {i
      packed} git object. Otherwise, we return an {!error}.
  *)

  val read_s : t -> Hash.t -> (Value.t, error) result Lwt.t
  (** [read_s state hash] is the same process than {!read_p} but we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val read : t -> Hash.t -> (Value.t, error) result Lwt.t
  (** Alias of {!read_s}. *)

  val size_p : ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    window:Inflate.window ->
    t -> Hash.t -> (int64, error) result Lwt.t
  (** [size_p ~ztmp ~dtmp ~raw ~window state hash] returns the size of the git
      object which respects the predicate [digest(object) = hash]. The size is how
      many byte(s) are needed to store the serialized (but not inflated) git object
      in bytes (without the header).

      As {!read_p}, {!size_p} needs some buffers. The client can use this
      function in a concurrency context when the buffers specified are not used
      by another process.

      As {!read_p}, {!size_p} can return an {!error}.
  *)

  val size_s : t -> Hash.t -> (int64, error) result Lwt.t
  (** [size_s state hash] is the same process than {!size_p} but we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val size : t -> Hash.t -> (int64, error) result Lwt.t
  (** Alias of {!size_s}. *)

  val raw_p : ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    window:Inflate.window ->
    t -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t) option Lwt.t
  (** [raw_p ~window ~ztmp ~dtmp ~raw state hash] can retrieve a git object
      available in the current git repository [state]. However, this function {b
      does not} de-serialize the git object and returns the kind of the object and
      the {i raw-data} inflated. Precisely, it decodes only the header. This
      function needs some buffers:

      {ul

      {- [window] is a buffer used by the {!Inflate} module}
      {- [ztmp] is a buffer used to store the inflated flow}
      {- [dtmp] is a buffer used by the decoder to save the inflated flow (and
      keep it for an alteration)}
      {- [raw] is a buffer used to store the input flow}}

      This function can be used in a concurrency context only if the specified
      buffers are not used by another process. This function follows the same
      scheme of allocation of {!Loose.raw_p} if the requested git object is a {i
      loose} git object or {!Pack.read_p} if the requested git object is a {i
      packed} git object. Otherwise, we return an {!error}.
  *)

  val raw_s : t -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t) option Lwt.t
  (** [raw_s state hash] is the same process than {!raw_p} but we use the
      state-defined buffer. That means the client can not use this function in a
      concurrency context with the same [state]. *)

  val raw : t -> Hash.t -> ([ `Commit | `Tree | `Tag | `Blob ] * Cstruct.t) option Lwt.t
  (** Alias of {!raw_s}. *)

  val contents : t -> ((Hash.t * Value.t) list, error) result Lwt.t
  (** [contents state] returns an associated list between the hash and its bind
      git object. This list contains all git objects available in the current git
      repository [state]. *)

  val buffer_window : t -> Inflate.window
  (** [buffer_window state] returns the state-defined [window]. *)

  val buffer_zl : t -> Cstruct.t
  (** [buffer_zl state] returns the state-defined buffer used to store any
      inflated flow. *)

  val buffer_de : t -> Cstruct.t
  (** [buffer_de state] returns the state-defined buffer used as internal buffer
      for any decoder. *)

  val buffer_io : t -> Cstruct.t
  (** [buffer_io state] returns the state-defined buffer used to store the input
      flow. *)

  val indexes : t -> Hash.t list
  (** [indexes state] returns all available IDX files in the current git repository [state]. *)

  val fold : t ->
    ('a -> ?name:Path.t -> length:int64 -> Hash.t -> Value.t -> 'a Lwt.t) ->
    path:Path.t -> 'a -> Hash.t -> 'a Lwt.t
  (** [fold f ~path acc hash] walks from the source [hash] in the {i sub-graph}.
      If the hash points to:

      {ul
      {- {!Value.Blob.t}: [f] is called only one time with the OCaml value of
      the {i blob}}
      {- {!Value.Tree.t}: [f] is called firstly with the OCaml value of the
      pointed {i tree} by the hash [hash]. Then, we {i iter} (and call [f] for
      each iterations) in the OCaml {i tree} value. For each iteration, we
      notice the pointed hash, the length of the pointed git object, the OCaml
      value of the pointed git object, and the [name] which is a composition
      between the current [path] and the notified name by the current OCaml {i
      tree} value. Then, we retrieve recursively all sub-git objects for each
      pointed hash (ascending path). [f] is never called more than one time for
      each hashes}
      {- {!Value.Commit.t}: [f] is called firstly with the OCaml value of the
      pointed {i commit} by the hash [hash]. Then, it follows recursively all
      parents of the current commit. Finally, it starts a [fold] inside the
      pointed root {i tree} git object of each {i commits} previously retrieved.
      [f] is never called more than one time for each hashes.}
      {- {!Value.Tag.t}: [f] is called firstly with the OCaml value of the
      pointed {i tag} by the hash [hash]. Then, it follows the git object
      pointed by the {i tag}.}}

      Any retrieved {!error} is missed.
  *)

  module Ref :
  sig
    type nonrec error =
      [ error
      | `Invalid_reference of Reference.t
      (** Appears when we can not retrieve the final value of the reference [ref]. *)
      ] (** The type error. *)

    val pp_error : error Fmt.t
    (** Pretty-printer of {!error}. *)

    val graph_p : t -> dtmp:Cstruct.t -> raw:Cstruct.t -> (Hash.t Reference.Map.t, error) result Lwt.t
    (** [graph_p state ~dtmp ~raw] returns a graph which contains all reference
        and its pointed final hash available in the current git repository
        [state]. This function needs some buffers:

        {ul
        {- [dtmp] is a buffer used by the decoder to save the input flow (and
        keep it for an alteration)}
        {- [raw] is a buffer used to store the input flow}}

        This function can be used in a concurrency context only if the specified
        buffers are not used by another process. If we encountered any {!error},
        we make it silent and continue the process.
    *)

    val graph : t -> (Hash.t Reference.Map.t, error) result Lwt.t
    (** [graph state] is the same process than {!graph_p} but we use the
        state-defined buffer. That means the client can not use this function in a
        concurrency context with the same [state]. *)

    val normalize : Hash.t Reference.Map.t -> Reference.head_contents -> (Hash.t, error) result Lwt.t
    (** [normalize graph ref] returns the final hash pointed by the reference
        [ref]. This function can return an error:

        {ul

        {- [`Invalid_reference ref] if, from the graph [graph], we don't find
        the final pointed hash. This case can appear if the graph is not
        complete or if the link is broken.}}
    *)

    val list_p : t
      -> dtmp:Cstruct.t
      -> raw:Cstruct.t
      -> ((Reference.t * Hash.t) list, error) result Lwt.t
    (** [list_p state ~dtmp ~raw] returns an associated list between reference
        and its bind hash. This function is the same than:

        > graph state >|= Reference.Map.fold (fun x acc -> x :: acc)

        As {!graph_p}, this function needs some buffers. And, as {!graph_p},
        this function can be used in a concurrency context only f the specified
        buffers are not used by another process. Finally, as {!graph_p}, if we
        encountered any {!error}, we make it silent and continue the process.

        NOTE: because we don't catch any error while the process, this function
        should not return a [result] type.
    *)

    val list_s      : t -> ((Reference.t * Hash.t) list, error) result Lwt.t
    (** [list_s state] is the same process than {!list_p} but we use the
        state-defined buffer. That means the client can not use this function in a
        concurrency context with the same [state]. *)

    val list        : t -> ((Reference.t * Hash.t) list, error) result Lwt.t
    (** Alias of {!list_s}. *)
  end
end

module Make
    (H : Ihash.S with type Digest.buffer = Cstruct.t
                  and type hex = string)
    (P : Path.S)
    (L : Lock.S)
    (FS : Fs.S with type path = P.t
                and type File.raw = Cstruct.t
                and type File.lock = L.t
                and type Mapper.raw = Cstruct.t)
    (Inflate : S.INFLATE)
    (Deflate : S.DEFLATE)
  : S with module Hash = H
       and module Path = P
       and module Lock = L
       and module Inflate = Inflate
       and module Deflate = Deflate
       and module FileSystem = FS
