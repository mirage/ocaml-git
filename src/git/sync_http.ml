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

module type CLIENT =
sig
  type headers
  type body
  type resp
  type meth
  type uri

  type +'a io

  val call : ?headers:headers -> ?body:body -> meth -> uri -> resp io
end

module type FLOW =
sig
  type raw

  type +'a io

  type i = (raw * int * int) option -> unit io
  type o = unit -> (raw * int * int) option io
end

module Lwt_cstruct_flow =
struct
  type raw = Cstruct.t

  type +'a io = 'a Lwt.t

  type i = (raw * int * int) option -> unit io
  type o = unit -> (raw * int * int) option io
end

module type S =
sig
  module Web : S.WEB
  module Client : CLIENT
    with type headers = Web.HTTP.headers
     and type meth = Web.HTTP.meth
     and type uri = Web.uri
     and type resp = Web.resp
  module Store : Minimal.S
  module Buffer : S.BUFFER

  module Decoder : Smart.DECODER
    with module Hash = Store.Hash

  module PACKDecoder : Unpack.P
    with module Hash = Store.Hash
     and module Inflate = Store.Inflate

  type error =
    [ `Decoder of Decoder.error
    | `DecoderFlow of string
    | `PackDecoder of PACKDecoder.error
    | `StorePack of Store.Pack.error
    | `Unresolved_object
    | `Apply of string ]

  val pp_error : error Fmt.t

  val ls :
       Store.t
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?port:int
    -> string -> string -> (Decoder.advertised_refs, error) result Lwt.t

  val clone :
       Store.t
    -> ?stdout:(Cstruct.t -> unit Lwt.t)
    -> ?stderr:(Cstruct.t -> unit Lwt.t)
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?port:int
    -> ?reference:Store.Reference.t
    -> string -> string -> (Store.Hash.t, error) result Lwt.t
end

module Make
    (K : Sync.CAPABILITIES)
    (W : S.WEB with type +'a io = 'a Lwt.t
                and type raw = Cstruct.t
                and type uri = Uri.t
                and type Request.body = Lwt_cstruct_flow.i
                and type Response.body = Lwt_cstruct_flow.o)
    (C : CLIENT with type +'a io = 'a W.io
                 and type headers = W.HTTP.headers
                 and type body = Lwt_cstruct_flow.o
                 and type meth = W.HTTP.meth
                 and type uri = W.uri
                 and type resp = W.resp)
    (G : Minimal.S with type Hash.Digest.buffer = Cstruct.t
                    and type Hash.hex = string)
    (B : S.BUFFER with type raw = string
                   and type fixe = Cstruct.t)
  : S with module Web = W
       and module Client = C
       and module Store = G
       and module Buffer = B
= struct
  module Web = W
  module Client = C
  module Store = G
  module Buffer = B

  module Decoder = Smart.Decoder(Store.Hash)
  module Encoder = Smart.Encoder(Store.Hash)

  module PACKDecoder = Unpack.MakePACKDecoder(Store.Hash)(Store.Inflate)

  type error =
    [ `Decoder of Decoder.error
    | `DecoderFlow of string
    | `PackDecoder of PACKDecoder.error
    | `StorePack of Store.Pack.error
    | `Unresolved_object
    | `Apply of string ]

  let pp_error ppf = function
    | `Decoder err       -> Fmt.pf ppf "(`Decoder %a)" Decoder.pp_error err
    | `DecoderFlow err   -> Fmt.pf ppf "(`DecoderFlow %s)" err
    | `PackDecoder err   -> Fmt.pf ppf "(`PackDecoder %a)" (Fmt.hvbox PACKDecoder.pp_error) err
    | `StorePack err     -> Fmt.pf ppf "(`StorePack %a)" Store.Pack.pp_error err
    | `Unresolved_object -> Fmt.pf ppf "`Unresolved_object"
    | `Apply err         -> Fmt.pf ppf "(`Apply %s)" err

  module Log =
  struct
    let src = Logs.Src.create "git.sync.http" ~doc:"logs git's sync http event"
    include (val Logs.src_log src : Logs.LOG)
  end

  let option_map_default f v = function
    | Some v -> f v
    | None -> v

  let default_stdout raw =
    Log.info (fun l -> l ~header:"populate:stdout" "%S" (Cstruct.to_string raw));
    Lwt.return ()

  let default_stderr raw =
    Log.err (fun l -> l ~header:"populate:stderr" "%S" (Cstruct.to_string raw));
    Lwt.return ()

  let populate git ?(stdout = default_stdout) ?(stderr = default_stderr) stream =
    let cstruct_copy cs =
      let ln = Cstruct.len cs in
      let rt = Cstruct.create ln in
      Cstruct.blit cs 0 rt 0 ln;
      rt
    in

    let open Lwt.Infix in
    let stream', push = Lwt_stream.create () in

    let rec dispatch () =
      stream () >>= function
      | Ok (`Out raw) ->
        stdout raw >>= dispatch
      | Ok (`Raw raw) ->
        Log.debug (fun l -> l ~header:"dispatch" "Retrieve a chunk of the PACK stream (length: %d)."
                      (Cstruct.len raw));
        push (Some (cstruct_copy raw));
        dispatch ()
      | Ok (`Err raw) ->
        stderr raw >>= dispatch
      | Ok `End ->
        Log.debug (fun l -> l ~header:"dispatch" "Retrieve end of the PACK stream.");
        push None;
        Lwt.return (Ok ())
      | Error err -> Lwt.return (Error (`Decoder err))
    in

    let open Lwt_result in

    let ( >!= ) = Lwt_result.bind_lwt_err in

    dispatch () >>= fun () ->
    (Store.Pack.from git (fun () -> Lwt_stream.get stream')
     >!= fun err -> Lwt.return (`StorePack err))

  let ls _ ?headers ?(https = false) ?port host path =
    let open Lwt.Infix in

    let scheme =
      if https then "https"
      else "http"
    in

    let uri =
      Uri.empty
      |> (fun uri -> Uri.with_scheme uri (Some scheme))
      |> (fun uri -> Uri.with_host uri (Some host))
      |> (fun uri -> Uri.with_path uri (String.concat "/" [ path; "info"; "refs" ]))
      |> (fun uri -> Uri.with_port uri port)
      |> (fun uri -> Uri.add_query_param uri ("service", [ "git-upload-pack" ]))
    in

    Log.debug (fun l -> l ~header:"ls" "Launch the GET request to %a."
                  Uri.pp_hum uri);

    let git_agent =
      List.fold_left (fun acc -> function `Agent s -> Some s | _ -> acc) None K.default
      |> function
      | Some git_agent -> git_agent
      | None -> raise (Invalid_argument "Expected an user agent in capabilities.")
    in

    let headers =
      option_map_default
        Web.HTTP.Headers.(def user_agent git_agent)
        Web.HTTP.Headers.(def user_agent git_agent empty)
        headers
    in

    Client.call ~headers `GET uri >>= fun resp ->

    let decoder = Decoder.decoder () in

    let rec consume stream ?keep state =
      match state with
      | Decoder.Ok v -> Lwt.return (Ok v)
      | Decoder.Error { err; _ } -> Lwt.return (Error err)
      | Decoder.Read { buffer; off; len; continue; } ->
        (match keep with
         | Some (raw, off', len') -> Lwt.return (Some (raw, off', len'))
         | None -> stream ()) >>= function
        | Some (raw, off', len') ->
          let len'' = min len len' in
          Cstruct.blit raw off' buffer off len'';

          if len' - len'' = 0
          then consume stream (continue len'')
          else consume stream ~keep:(raw, off' + len'', len' - len'') (continue len'')
        | None -> consume stream (continue 0)
    in

    let ( >!= ) = Lwt_result.bind_lwt_err in

    consume (Web.Response.body resp) (Decoder.decode decoder (Decoder.HttpReferenceDiscovery "git-upload-pack"))
    >!= (fun err -> Lwt.return (`Decoder err))

  let clone git ?stdout ?stderr ?headers ?(https = false) ?port ?(reference = Store.Reference.head) host path =
    let open Lwt.Infix in

    let scheme, port =
      if https then match port with Some x -> "https", x | None -> "https", 443
      else match port with Some x -> "http", x | None -> "http", 80
    in

    let uri =
      Uri.empty
      |> (fun uri -> Uri.with_scheme uri (Some scheme))
      |> (fun uri -> Uri.with_host uri (Some host))
      |> (fun uri -> Uri.with_path uri (String.concat "/" [ path; "info"; "refs" ]))
      |> (fun uri -> Uri.with_port uri (Some port))
      |> (fun uri -> Uri.add_query_param uri ("service", [ "git-upload-pack" ]))
    in

    Log.debug (fun l -> l ~header:"clone" "Launch the GET request to %a."
                  Uri.pp_hum uri);

    let git_agent =
      List.fold_left (fun acc -> function `Agent s -> Some s | _ -> acc) None K.default
      |> function
      | Some git_agent -> git_agent
      | None -> raise (Invalid_argument "Expected an user agent in capabilities.")
    in

    let headers =
      option_map_default
        Web.HTTP.Headers.(def user_agent git_agent)
        Web.HTTP.Headers.(def user_agent git_agent empty)
        headers
    in

    Client.call ~headers `GET uri >>= fun resp ->

    let decoder = Decoder.decoder () in
    let encoder = Encoder.encoder () in

    let rec consume stream ?keep state =
      match state with
      | Decoder.Ok v -> Lwt.return (Ok v)
      | Decoder.Error { err; _ } -> Lwt.return (Error err)
      | Decoder.Read { buffer; off; len; continue; } ->
        (match keep with
         | Some (raw, off', len') -> Lwt.return (Some (raw, off', len'))
         | None -> stream ()) >>= function
        | Some (raw, off', len') ->
          let len'' = min len len' in
          Cstruct.blit raw off' buffer off len'';

          if len' - len'' = 0
          then consume stream (continue len'')
          else consume stream ~keep:(raw, off' + len'', len' - len'') (continue len'')
        | None -> consume stream (continue 0)
    in

    consume (Web.Response.body resp) (Decoder.decode decoder (Decoder.HttpReferenceDiscovery "git-upload-pack")) >>= function
    | Error err ->
      Log.err (fun l -> l ~header:"clone" "The HTTP decoder returns an error: %a." Decoder.pp_error err);
      Lwt.return (Error (`Decoder err))
    | Ok v ->
      let common = List.filter (fun x -> List.exists ((=) x) K.default) v.Decoder.capabilities in

      let (expect, _, _) =
        List.find
          (fun (_, refname, _) -> Store.Reference.(equal (of_string refname) reference))
          v.Decoder.refs
      in

      Log.debug (fun l -> l ~header:"clone" "%a is %a"
                    Store.Reference.pp reference
                    Store.Hash.pp expect);

      let producer state =
        let state' = ref (fun () -> state) in

        let go () = match !state' () with
          | Encoder.Write { buffer; off; len; continue; } ->
            state' := (fun () -> continue len);
            Lwt.return (Some (buffer, off, len))
          | Encoder.Ok () -> Lwt.return None
        in go
      in

      let req =
        Web.Request.v
          `POST
          ~path:[ path; "git-upload-pack" ]
          Web.HTTP.Headers.(def content_type "application/x-git-upload-pack-request" headers)
          (fun _ -> Lwt.return ())
      in

      Client.call
        ~headers:(Web.Request.headers req |> Web.HTTP.Headers.merge headers)
        ~body:(producer (Encoder.encode encoder
                           (`HttpUploadRequest { Encoder.want = expect, []
                                               ; capabilities = K.default
                                               ; shallow = []
                                               ; deep = None })))
        (Web.Request.meth req)
        (Web.Request.uri req
         |> (fun uri -> Uri.with_scheme uri (Some "http"))
         |> (fun uri -> Uri.with_host uri (Some host))
         |> (fun uri -> Uri.with_port uri (Some port)))
      >>= fun resp ->

      let sideband =
        if List.exists ((=) `Side_band_64k) common
        then `Side_band_64k
        else if List.exists ((=) `Side_band) common
        then `Side_band
        else `No_multiplexe
      in

      consume
        (Web.Response.body resp)
        (Decoder.decode decoder (Decoder.HttpPACK ((function Decoder.NAK -> true | _ -> false), sideband)))
      >>= function
      | Ok first ->
        let consumed = ref false in

        let stream () =
          if not !consumed
          then (consumed := true; Lwt.return (Ok first))
          else consume (Web.Response.body resp) (Decoder.decode decoder (Decoder.PACK sideband))
        in

        Lwt_result.(populate ?stdout ?stderr git stream >>= fun _ -> Lwt.return (Ok expect))
      | Error err ->
        Log.err (fun l -> l ~header:"clone" "The HTTP decoder returns an error: %a." Decoder.pp_error err);
        Lwt.return (Error (`Decoder err))
end

