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
  module Web    : S.WEB
  module Client : CLIENT
    with type headers = Web.HTTP.headers
     and type meth = Web.HTTP.meth
     and type uri = Web.uri
     and type resp = Web.resp
  module Store  : Minimal.S
    with type Hash.Digest.buffer = Cstruct.t
     and type Hash.hex = string
  module Buffer : S.BUFFER

  module Decoder : Smart.DECODER
    with module Hash = Store.Hash
  module Encoder : Smart.ENCODER
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
    | `Clone of string
    | `Apply of string ]

  val pp_error : error Fmt.t

  val ls :
       Store.t
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> ?port:int
    -> string -> string -> (Decoder.advertised_refs, error) result Lwt.t

  val fetch :
       Store.t
    -> ?shallow:Store.Hash.t list
    -> ?stdout:(Cstruct.t -> unit Lwt.t)
    -> ?stderr:(Cstruct.t -> unit Lwt.t)
    -> ?headers:Web.HTTP.headers
    -> ?https:bool
    -> negociate:((Decoder.acks -> 'state -> ([ `Ready | `Done | `Again of Store.Hash.t list ] * 'state) Lwt.t) * 'state)
    -> has:Store.Hash.t list
    -> want:((Store.Hash.t * string * bool) list -> (Store.Reference.t * Store.Hash.t) list Lwt.t)
    -> ?deepen:[ `Depth of int | `Timestamp of int64 | `Ref of string ]
    -> ?port:int
    -> string -> string -> ((Store.Reference.t * Store.Hash.t) list * int, error) result Lwt.t

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
  : S with module Web     = W
       and module Client  = C
       and module Store   = G
       and module Buffer  = B
= struct
  module Web    = W
  module Client = C
  module Store  = G
  module Buffer = B

  module Decoder
    = Smart.Decoder(Store.Hash)
  module Encoder
    = Smart.Encoder(Store.Hash)

  module PACKDecoder
    = Unpack.MakePACKDecoder(Store.Hash)(Store.Inflate)

  type error =
    [ `Decoder of Decoder.error
    | `DecoderFlow of string
    | `PackDecoder of PACKDecoder.error
    | `StorePack of Store.Pack.error
    | `Unresolved_object
    | `Clone of string
    | `Apply of string ]

  let pp_error ppf = function
    | `Decoder err       -> Fmt.pf ppf "(`Decoder %a)" Decoder.pp_error err
    | `DecoderFlow err   -> Fmt.pf ppf "(`DecoderFlow %s)" err
    | `PackDecoder err   -> Fmt.pf ppf "(`PackDecoder %a)" (Fmt.hvbox PACKDecoder.pp_error) err
    | `StorePack err     -> Fmt.pf ppf "(`StorePack %a)" Store.Pack.pp_error err
    | `Unresolved_object -> Fmt.pf ppf "`Unresolved_object"
    | `Clone err         -> Fmt.pf ppf "(`Clone %s)" err
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

  let producer state =
    let state' = ref (fun () -> state) in

    let go () = match !state' () with
      | Encoder.Write { buffer; off; len; continue; } ->
        state' := (fun () -> continue len);
        Lwt.return (Some (buffer, off, len))
      | Encoder.Ok () -> Lwt.return None
    in go

  let rec consume stream ?keep state =
    let open Lwt.Infix in

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

  let ls _ ?headers ?(https = false) ?port host path =
    let open Lwt.Infix in

    let scheme = if https then "https" else "http" in
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

    let ( >!= ) = Lwt_result.bind_lwt_err in

    consume (Web.Response.body resp) (Decoder.decode decoder (Decoder.HttpReferenceDiscovery "git-upload-pack"))
    >!= (fun err -> Lwt.return (`Decoder err))

  let fetch git ?(shallow = []) ?stdout ?stderr ?headers ?(https = false) ~negociate:(negociate, nstate) ~has ~want ?deepen ?port host path =
    let open Lwt.Infix in

    let scheme = if https then "https" else "http" in
    let uri =
      Uri.empty
      |> (fun uri -> Uri.with_scheme uri (Some scheme))
      |> (fun uri -> Uri.with_host uri (Some host))
      |> (fun uri -> Uri.with_path uri (String.concat "/" [ path; "info"; "refs" ]))
      |> (fun uri -> Uri.with_port uri port)
      |> (fun uri -> Uri.add_query_param uri ("service", [ "git-upload-pack" ]))
    in

    Log.debug (fun l -> l ~header:"fetch" "Launch the GET request to %a."
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
    let keeper = Lwt_mvar.create has in

    consume (Web.Response.body resp) (Decoder.decode decoder (Decoder.HttpReferenceDiscovery "git-upload-pack")) >>= function
    | Error err ->
      Log.err (fun l -> l ~header:"fetch" "The HTTP decoder returns an error: %a." Decoder.pp_error err);
      Lwt.return (Error (`Decoder err))
    | Ok refs ->
      let common = List.filter (fun x -> List.exists ((=) x) K.default) refs.Decoder.capabilities in

      let sideband =
        if List.exists ((=) `Side_band_64k) common
        then `Side_band_64k
        else if List.exists ((=) `Side_band) common
        then `Side_band
        else `No_multiplexe
      in

      let ack_mode =
        if List.exists ((=) `Multi_ack_detailed) common
        then `Multi_ack_detailed
        else if List.exists ((=) `Multi_ack) common
        then `Multi_ack
        else `Ack
      in

      want refs.Decoder.refs >>= function
      | [] -> Lwt.return (Ok ([], 0))
      | first :: rest ->
        let negociation_request final has =
          Log.debug (fun l -> l ~header:"fetch" "Send a POST negociation request (done:%b): %a."
                        final (Fmt.Dump.list Store.Hash.pp) has);

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
                               (`HttpUploadRequest (final,
                                                    { Encoder.want = snd first, List.map snd rest
                                                    ; capabilities = K.default
                                                    ; shallow
                                                    ; deep = deepen
                                                    ; has }))))
            (Web.Request.meth req)
            (Web.Request.uri req
             |> (fun uri -> Uri.with_scheme uri (Some scheme))
             |> (fun uri -> Uri.with_host uri (Some host))
             |> (fun uri -> Uri.with_port uri port))
        in

        negociation_request false has >>= fun resp ->

        let next resp =
          consume (Web.Response.body resp)
            (Decoder.decode decoder Decoder.NegociationResult) >>= function
          | Error err -> Lwt.return (Error (`Decoder err))
          | Ok _ -> (* TODO: check negociation result. *)
            let stream () = consume (Web.Response.body resp) (Decoder.decode decoder (Decoder.PACK sideband)) in
            Lwt_result.(populate ?stdout ?stderr git stream >>= fun (_, n) -> Lwt.return (Ok (first :: rest, n)))
        in

        let rec loop ~final state resp = match final with
          | true -> next resp
          | false ->
            Lwt_mvar.take keeper >>= fun has -> Lwt_mvar.put keeper has >>= fun () ->
            consume (Web.Response.body resp)
              (Decoder.decode decoder (Decoder.Negociation (has, ack_mode))) >>= function
            | Error err ->
              Lwt.return (Error (`Decoder err))
            | Ok acks ->
              negociate acks state >>= function
              | `Ready, _ -> next resp
              | `Again has', state ->
                Lwt_mvar.take keeper >>= fun has ->
                let has = has @ has' in
                Lwt_mvar.put keeper has >>= fun () ->

                negociation_request false has >>= loop ~final:false state
              | `Done, _ -> next resp
        in

        loop ~final:(List.length has = 0) nstate resp

  let clone git ?stdout ?stderr ?headers ?(https = false) ?port ?(reference = Store.Reference.head) host path =
    let want refs =
      Lwt.try_bind
        (fun () -> Lwt_list.find_s (fun (_, refname, _) -> Lwt.return (Store.Reference.(equal (of_string refname) reference))) refs)
        (fun (hash, _, _) -> Lwt.return [ reference, hash ])
        (fun _ -> Lwt.return [])
    in

    let open Lwt.Infix in

    fetch git ?stdout ?stderr ?headers ~https ~negociate:((fun _ () -> Lwt.return (`Done, ())), ())
      ~has:[]
      ~want
      ?port host path
    >>= function
    | Ok ([ _, hash ], _) -> Lwt.return (Ok hash)
    | Ok (expect, _) ->
      Lwt.return (Error (`Clone (Fmt.strf "Unexpected result: %a."
                                   (Fmt.hvbox (Fmt.Dump.list (Fmt.Dump.pair Store.Reference.pp Store.Hash.pp)))
                                   expect)))
    | Error _ as err -> Lwt.return err
end

