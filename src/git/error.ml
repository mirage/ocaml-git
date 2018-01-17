[@@@warning "-32"]

let v v = Error v

type not_found =
  [ `Not_found ]

let err_not_found = `Not_found

let pp_not_found ppf = function
  | `Not_found -> Fmt.pf ppf "Not found"

type never = [ `Never ]

let pp_never _ppf = assert false

module FS (FS: S.FS) =
struct

  (* First stage: only error. *)

  type t0 =
    [ `SystemFile of FS.error
    | `SystemDir of FS.error
    | `SystemMap of FS.error ]

  let t0_pp_error ppf = function
    | `SystemFile err ->
      Fmt.pf ppf "Got an file error: %a"
        FS.pp_error err
    | `SystemDirectory err ->
      Fmt.pf ppf "Got a directory error: %a"
        FS.pp_error err
    | `SystemMap err ->
      Fmt.pf ppf "Got a mapper error: %a"
        FS.pp_error err

  let err_sys_file err = `SystemFile err
  let err_sys_dir err = `SystemFile err
  let err_sys_map err = `SystemMap err

  (* Second stage: error with path. *)

  type t1 =
    [ `File of Fpath.t * FS.error
    | `Dir of Fpath.t * FS.error
    | `Map of Fpath.t * FS.error ]

  let t1_pp_error ppf = function
    | `File (path, error)->
      Fmt.pf ppf "Got an error while processing %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Dir (path, error) ->
      Fmt.pf ppf "Got an error while processing %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Map (path, error ) ->
      Fmt.pf ppf "Got an error while processing %a: %a"
        Fpath.pp path
        FS.pp_error error

  let err_file path error = `File (path, error)
  let err_dir path error = `Dir (path, error)
  let err_map path error = `Map (path, error)

  (* Third stage: error with path and process. *)

  type t2 =
    [ `Read   of Fpath.t * FS.error
    | `Write  of Fpath.t * FS.error
    | `Open   of Fpath.t * FS.error
    | `Close  of Fpath.t * FS.error
    | `Create of Fpath.t * FS.error
    | `Length of Fpath.t * FS.error
    | `Map    of Fpath.t * FS.error
    | `Delete of Fpath.t * FS.error ]

  let t2_pp_error ppf = function
    | `Read (path, error) ->
      Fmt.pf ppf "Got an error while reading %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Write (path, error) ->
      Fmt.pf ppf "Got an error while writing %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Close (path, error) ->
      Fmt.pf ppf "Got an error while closing %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Open (path, error) ->
      Fmt.pf ppf "Got an error while opening %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Create (path, error) ->
      Fmt.pf ppf "Got an error while creating %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Length (path, error) ->
      Fmt.pf ppf "Got an error when we get length %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Map (path, error) ->
      Fmt.pf ppf "Got an error while mapping %a: %a"
        Fpath.pp path
        FS.pp_error error
    | `Delete (path, error) ->
      Fmt.pf ppf "Got and error when we delete %a: %a"
        Fpath.pp path
        FS.pp_error error

  let err_read   path error = `Read (path, error)
  let err_write  path error = `Write (path, error)
  let err_close  path error = `Close (path, error)
  let err_open   path error = `Open (path, error)
  let err_create path error = `Create (path, error)
  let err_length path error = `Length (path, error)
  let err_map    path error = `Map (path, error)
  let err_delete path error = `Delete (path, error)

  (* Fourth stage: special errors. *)

  type t3 =
    [ `Move  of Fpath.t * Fpath.t * FS.error
    | `Stack of Fpath.t ]

  let t3_pp_error ppf = function
    | `Move (a, b, e) ->
      Fmt.pf ppf "Got an error while moving %a to %a: %a"
        Fpath.pp a
        Fpath.pp b
        FS.pp_error e
    | `Stack path ->
      Fmt.pf ppf "Impossible to write (limit reached) on %a"
        Fpath.pp path

  let err_move  a b err = `Move (a, b, err)
  let err_stack path = `Stack path
end

module Inf (Inf: S.INFLATE) =
struct

  type t0 =
    [ `Inflate of Inf.error ]

  let t0_pp_error ppf = function
    | `Inflate err ->
      Fmt.pf ppf "Got an error while inflating: %a"
        Inf.pp_error err

  let err_inflate err = `Inflate err

  type t1 =
    [ `Inflate_file of Fpath.t * Inf.error ]

  let t1_of_t0 path = function
    | `Inflate err -> `Inflate_file (path, err)

  let t1_pp_error ppf = function
    | `Inflate_file (path, err) ->
      Fmt.pf ppf "Got an error while inflating %a: %a"
        Fpath.pp path
        Inf.pp_error err

  let err_inflate_file path err = `Inflate_file (path, err)
end

module Def (Def: S.DEFLATE) =
struct

  type t0 =
    [ `Deflate of Def.error ]

  let pp_error ppf = function
    | `Deflate err ->
      Fmt.pf ppf "Got an error while deflating: %a"
        Deflate.pp_error err

  let err_deflate err = `Deflate err

  type t1 =
    [ `Deflate_file of Fpath.t * Def.error ]

  let t1_of_t0 path = function
    | `Deflate err -> `Deflate_file (path, err)

  let t1_pp_error ppf = function
    | `Deflate_file (path, err) ->
      Fmt.pf ppf "Got an error while deflating %a: %a"
        Fpath.pp path
        Def.pp_error err

  let err_deflate_file path err = `Deflate_file (path, err)
end

module Angstrom =
struct

  type t0 =
    [ `Decoder of info
    | `Result of Cstruct.t * string ]
  and info = { committed : int; path : string list; error : string }

  let t0_pp_error ppf = function
    | `Decoder { committed; path; error; }->
      Fmt.pf ppf "Got an error while decoding (%a, committed: %d): %s\n"
        Fmt.(Dump.list string) path
        committed
        error
    | `Result (buf, err) ->
      Fmt.pf ppf "%s\n" err;
      Fmt.pf ppf "%a\n"
        (Minienc.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len) buf

  let err_decode (committed, path, error) = `Decoder { committed; path; error; }
  let err_result buf err = `Result (buf, err)

  type t1 =
    [ `Decoder_stream of Cstruct.t * info ]

  let t1_pp_error ppf = function
    | `Decoder_stream (buf, info) ->
      Fmt.pf ppf "Got an error while decoding chunk (%a): %s\n"
        Fmt.(Dump.list string) info.path
        info.error;
      Fmt.pf ppf "%a\n"
        (Minienc.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len)
        (Cstruct.shift buf info.committed)

  let err_decode_stream buf (committed, path, error) =
    `Decoder_stream (buf, { committed; path; error; })

  type t2 =
    [ `Decoder_file of Fpath.t * info ]

  let t2_of_t0 path = function
    | `Decoder info -> `Decoder_file (path, info)
    | `Result _ -> assert false

  let t2_pp_error ppf = function
    | `Decoder_file (path, info) ->
      Fmt.pf ppf "Got an error while decoding file %a at the byte %d: %s"
        Fpath.pp path info.committed info.error

  let err_decode_file file_path (committed, path, error) =
    `Decoder_file (file_path, { committed; path; error; })
end

module Decoder =
struct

  type t0 =
    [ Angstrom.t0
    | `Too_big of (int * int)
    | `Malicious ]

  let t0_pp_error ppf = function
    | #Angstrom.t0 as err ->
      Angstrom.t0_pp_error ppf err
    | `Too_big (has, max) ->
      Fmt.pf ppf "Input to decode is too big (%d bytes), \
                  we accept only equal or lower than %d bytes"
        has max
    | `Malicious ->
      Fmt.pf ppf "Input to decode does not respect assertion, \
                  it may be malicious"

  let err_decode = Angstrom.err_decode
  let err_result = Angstrom.err_result
  let err_too_big has max = `Too_big (has, max)
  let err_malicious = `Malicious

  type t2 =
    [ Angstrom.t2
    | `Chunk_too_big of Fpath.t * int * int
    | `Malicious_file of Fpath.t ]

  let t2_of_t0 path = function
    | #Angstrom.t0 as t0 -> Angstrom.t2_of_t0 path t0
    | `Too_big (has, max) -> `Chunk_too_big (path, has, max)
    | `Malicious -> `Malicious_file path

  let t2_pp_error ppf = function
    | #Angstrom.t2 as err ->
      Angstrom.t2_pp_error ppf err
    | `Chunk_too_big (path, has, max) ->
      Fmt.pf ppf "Chunk loaded from %a is too big (%d bytes) for the decoder (max: %d bytes)"
        Fpath.pp path has max
    | `Malicious_file path ->
      Fmt.pf ppf "%a could be a malicious file"
        Fpath.pp path
end
