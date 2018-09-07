let v v = Error v

type not_found = [`Not_found]

let err_not_found = `Not_found
let pp_not_found ppf = function `Not_found -> Fmt.pf ppf "Not found"

type never = [`Never]

let pp_never _ppf = assert false

module FS = struct
  type 'e t =
    [ `SystemFile of 'e
    | `SystemDirectory of 'e
    | `SystemMap of 'e
    | `File of Fpath.t * 'e
    | `Dir of Fpath.t * 'e
    | `Map of Fpath.t * 'e
    | `Read of Fpath.t * 'e
    | `Write of Fpath.t * 'e
    | `Open of Fpath.t * 'e
    | `Close of Fpath.t * 'e
    | `Create of Fpath.t * 'e
    | `Length of Fpath.t * 'e
    | `Map of Fpath.t * 'e
    | `Delete of Fpath.t * 'e
    | `Move of Fpath.t * Fpath.t * 'e
    | `Stack of Fpath.t ]

  let pp_error pp_fs_error ppf = function
    | `SystemFile err -> Fmt.pf ppf "Got an file error: %a" pp_fs_error err
    | `SystemDirectory err ->
        Fmt.pf ppf "Got a directory error: %a" pp_fs_error err
    | `SystemMap err -> Fmt.pf ppf "Got a mapper error: %a" pp_fs_error err
    | `File (path, error) ->
        Fmt.pf ppf "Got an error while processing %a: %a" Fpath.pp path
          pp_fs_error error
    | `Dir (path, error) ->
        Fmt.pf ppf "Got an error while processing %a: %a" Fpath.pp path
          pp_fs_error error
    | `Map (path, error) ->
        Fmt.pf ppf "Got an error while processing %a: %a" Fpath.pp path
          pp_fs_error error
    | `Read (path, error) ->
        Fmt.pf ppf "Got an error while reading %a: %a" Fpath.pp path
          pp_fs_error error
    | `Write (path, error) ->
        Fmt.pf ppf "Got an error while writing %a: %a" Fpath.pp path
          pp_fs_error error
    | `Close (path, error) ->
        Fmt.pf ppf "Got an error while closing %a: %a" Fpath.pp path
          pp_fs_error error
    | `Open (path, error) ->
        Fmt.pf ppf "Got an error while opening %a: %a" Fpath.pp path
          pp_fs_error error
    | `Create (path, error) ->
        Fmt.pf ppf "Got an error while creating %a: %a" Fpath.pp path
          pp_fs_error error
    | `Length (path, error) ->
        Fmt.pf ppf "Got an error when we get length %a: %a" Fpath.pp path
          pp_fs_error error
    | `Delete (path, error) ->
        Fmt.pf ppf "Got and error when we delete %a: %a" Fpath.pp path
          pp_fs_error error
    | `Move (a, b, e) ->
        Fmt.pf ppf "Got an error while moving %a to %a: %a" Fpath.pp a Fpath.pp
          b pp_fs_error e
    | `Stack path ->
        Fmt.pf ppf "Impossible to write (limit reached) on %a" Fpath.pp path

  let err_sys_file err = `SystemFile err
  let err_sys_dir err = `SystemFile err
  let err_sys_map err = `SystemMap err
  let err_file path error = `File (path, error)
  let err_dir path error = `Dir (path, error)
  let err_read path error = `Read (path, error)
  let err_write path error = `Write (path, error)
  let err_close path error = `Close (path, error)
  let err_open path error = `Open (path, error)
  let err_create path error = `Create (path, error)
  let err_length path error = `Length (path, error)
  let err_map path error = `Map (path, error)
  let err_delete path error = `Delete (path, error)
  let err_move a b err = `Move (a, b, err)
  let err_stack path = `Stack path
end

module Inf = struct
  type 'e t = [`Inflate of 'e | `Inflate_file of Fpath.t * 'e]

  let pp_error pp_inflate_error ppf = function
    | `Inflate err ->
        Fmt.pf ppf "Got an error while inflating: %a" pp_inflate_error err
    | `Inflate_file (path, err) ->
        Fmt.pf ppf "Got an error while inflating %a: %a" Fpath.pp path
          pp_inflate_error err

  let err_inflate err = `Inflate err
  let err_inflate_file path err = `Inflate_file (path, err)

  let with_path path = function
    | `Inflate err -> `Inflate_file (path, err)
    | `Inflate_file _ as t -> t
end

module Def = struct
  type 'e t = [`Deflate of 'e | `Deflate_file of Fpath.t * 'e]

  let pp_error pp_deflate_error ppf = function
    | `Deflate err ->
        Fmt.pf ppf "Got an error while deflating: %a" pp_deflate_error err
    | `Deflate_file (path, err) ->
        Fmt.pf ppf "Got an error while deflating %a: %a" Fpath.pp path
          pp_deflate_error err

  let err_deflate err = `Deflate err
  let err_deflate_file path err = `Deflate_file (path, err)

  let with_path path = function
    | `Deflate err -> `Deflate_file (path, err)
    | `Deflate_file _ as t -> t
end

module Angstrom = struct
  type t =
    [ `Decoder of info
    | `Result of Cstruct.t * string
    | `Decoder_stream of Cstruct.t * info
    | `Decoder_file of Fpath.t * info ]

  and info = {committed: int; path: string list; error: string}

  let pp_error ppf = function
    | `Decoder {committed; path; error} ->
        Fmt.pf ppf "Got an error while decoding (%a, committed: %d): %s\n"
          Fmt.(Dump.list string)
          path committed error
    | `Result (buf, err) ->
        Fmt.pf ppf "%s\n" err ;
        Fmt.pf ppf "%a\n"
          (Encore.Lole.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len)
          buf
    | `Decoder_stream (buf, info) ->
        Fmt.pf ppf "Got an error while decoding chunk (%a): %s\n"
          Fmt.(Dump.list string)
          info.path info.error ;
        Fmt.pf ppf "%a\n"
          (Encore.Lole.pp_scalar ~get:Cstruct.get_char ~length:Cstruct.len)
          (Cstruct.shift buf info.committed)
    | `Decoder_file (path, info) ->
        Fmt.pf ppf "Got an error while decoding file %a at the byte %d: %s"
          Fpath.pp path info.committed info.error

  let err_decode (committed, path, error) = `Decoder {committed; path; error}
  let err_result buf err = `Result (buf, err)

  let err_decode_stream buf (committed, path, error) =
    `Decoder_stream (buf, {committed; path; error})

  let err_decode_file file_path (committed, path, error) =
    `Decoder_file (file_path, {committed; path; error})

  let with_path path = function
    | `Decoder_stream (_, info) | `Decoder info -> `Decoder_file (path, info)
    | `Decoder_file _ as res -> res
    | `Result _ -> assert false
end

module Decoder = struct
  type t =
    [ Angstrom.t
    | `Too_big of int * int
    | `Malicious
    | `Chunk_too_big of Fpath.t * int * int
    | `Malicious_file of Fpath.t ]

  let pp_error ppf = function
    | #Angstrom.t as err -> Angstrom.pp_error ppf err
    | `Too_big (has, max) ->
        Fmt.pf ppf
          "Input to decode is too big (%d bytes), we accept only equal or \
           lower than %d bytes"
          has max
    | `Malicious ->
        Fmt.pf ppf
          "Input to decode does not respect assertion, it may be malicious"
    | `Chunk_too_big (path, has, max) ->
        Fmt.pf ppf
          "Chunk loaded from %a is too big (%d bytes) for the decoder (max: \
           %d bytes)"
          Fpath.pp path has max
    | `Malicious_file path ->
        Fmt.pf ppf "%a could be a malicious file" Fpath.pp path

  let err_decode = Angstrom.err_decode
  let err_result = Angstrom.err_result
  let err_too_big has max = `Too_big (has, max)
  let err_malicious = `Malicious

  let with_path path = function
    | #Angstrom.t as t -> Angstrom.with_path path t
    | `Too_big (has, max) -> `Chunk_too_big (path, has, max)
    | `Malicious -> `Malicious_file path
    | `Malicious_file _ as t -> t
    | `Chunk_too_big _ as t -> t
end
