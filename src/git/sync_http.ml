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

[@@@warning "-32"]
[@@@warning "-27"]

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
    | `Unresolved_object
    | `Apply of string ]

  val pp_error : error Fmt.t

  val clone :
       Store.t
    -> ?headers:Web.HTTP.headers
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
    | `Unresolved_object
    | `Apply of string ]

  let pp_error ppf = function
    | `Decoder err -> Fmt.pf ppf "(`Decoder %a)" Decoder.pp_error err
    | `DecoderFlow err -> Fmt.pf ppf "(`DecoderFlow %s)" err
    | `PackDecoder err -> Fmt.pf ppf "(`PackDecoder %a)" (Fmt.hvbox PACKDecoder.pp_error) err
    | `Unresolved_object -> Fmt.pf ppf "`Unresolved_object"
    | `Apply err -> Fmt.pf ppf "(`Apply %s)" err

  module Log =
  struct
    let src = Logs.Src.create "git.sync.http" ~doc:"logs git's sync http event"
    include (val Logs.src_log src : Logs.LOG)
  end

  type optimized_hunk =
    | Insert of (int * int)
    | Copy of (int * int)

  let pp_optimized_hunk ppf = function
    | Insert (off, len) ->
      Fmt.pf ppf "(Insert { @[<hov>length = %d;@ \
                  offset = %d;@] })"
        len off
    | Copy (off, len) ->
      Fmt.pf ppf "(Copy { @[<hov>length = %d;@ \
                  offset = %d;@] })"
        len off

  let option_map f v = match v with Some v -> Some (f v) | None -> None
  let option_map_default f d v = match v with Some v -> f v | None -> d
  let option_default v = function Some v -> v | None -> v

  module Revidx = Map.Make(Int64)

  let default_stdout raw =
    Log.info (fun l -> l ~header:"populate:stdout" "%S" (Cstruct.to_string raw));
    Lwt.return ()

  let default_stderr raw =
    Log.err (fun l -> l ~header:"populate:stderr" "%S" (Cstruct.to_string raw));
    Lwt.return ()

  let populate git ?(stdout = default_stdout) ?(stderr = default_stderr) stream =
    let cstruct_copy cs =
      let ln = Cstruct.len cs in
      let rs = Cstruct.create ln in

      Cstruct.blit cs 0 rs 0 ln;
      rs
    in

    let apply hunks_descr hunks buffer_hunks source target =
      if Cstruct.len target < hunks_descr.PACKDecoder.H.target_length
      then raise (Invalid_argument "apply");

      let target_length =
        List.fold_left
          (fun acc -> function
             | Insert (off, len) ->
               Cstruct.blit buffer_hunks off target acc len; acc + len
             | Copy (off, len) ->
               Cstruct.blit source off target acc len; acc + len)
          0 hunks
      in

      if target_length = hunks_descr.PACKDecoder.H.target_length
      then Ok (Cstruct.sub target 0 target_length)
      else Error (Fmt.strf "Bad undelta-ification (result: %d, expect: %d)" target_length hunks_descr.PACKDecoder.H.target_length)
    in

    let k2k = function
      | PACKDecoder.Commit -> `Commit
      | PACKDecoder.Tag -> `Tag
      | PACKDecoder.Tree -> `Tree
      | PACKDecoder.Blob -> `Blob
      | PACKDecoder.Hunk _ -> raise (Invalid_argument "k2k")
    in

    let open Lwt.Infix in

    let empty        = Cstruct.create 0 in
    let buffer       = Buffer.create 0x800 in
    let buffer_hunks = Buffer.create 0x800 in
    let queue        = Queue.create () in

    let rec go ~revidx ?(src = empty) ?hunks state =
      match PACKDecoder.eval src state with
      | `Await state ->
        (stream () >>= function
          | Ok (`Out raw) ->
            stdout raw >>= fun () -> go ~revidx ?hunks (PACKDecoder.refill 0 0 state)
          | Ok (`Raw raw) ->
            go ~revidx ~src:raw ?hunks (PACKDecoder.refill 0 (Cstruct.len raw) state)
          | Ok (`Err raw) ->
            stderr raw >>= fun () -> go ~revidx ?hunks (PACKDecoder.refill 0 0 state)
          | Ok `End ->
            Lwt.return (Error (`DecoderFlow "Unexpected end of stream"))
          | Error err -> Lwt.return (Error (`Decoder err)))
      | `End _ ->
        Lwt.return (Ok ())
      | `Error (_, err) ->
        Log.err (fun l -> l ~header:"populate" "The PACK decoder returns an error: %a." PACKDecoder.pp_error err);
        Lwt.return (Error (`PackDecoder err))
      | `Flush state ->
        let o, n = PACKDecoder.output state in

        Buffer.add buffer (Cstruct.sub o 0 n);
        go ~revidx ~src (PACKDecoder.flush 0 (Cstruct.len o) state)
      | `Hunk (state, hunk) ->
        let hunks = match hunks, hunk with
          | Some hunks, PACKDecoder.H.Insert raw ->
            let off = Buffer.has buffer_hunks in
            Buffer.add buffer_hunks raw;

            (Insert (off, Cstruct.len raw) :: hunks)
          | Some hunks, PACKDecoder.H.Copy (off, len) ->
            (Copy (off, len) :: hunks)
          | None, PACKDecoder.H.Insert raw ->
            let off = Buffer.has buffer_hunks in
            Buffer.add buffer_hunks raw;

            [ Insert (off, Cstruct.len raw) ]
          | None, PACKDecoder.H.Copy (off, len) ->
            [ Copy (off, len) ]
        in

        go ~revidx ~src ~hunks (PACKDecoder.continue state)
      | `Object state ->
        (match PACKDecoder.kind state with
         | (PACKDecoder.Commit
           | PACKDecoder.Tag
           | PACKDecoder.Tree
           | PACKDecoder.Blob) as kind ->

           let raw = Buffer.contents buffer |> Cstruct.of_string in
           Buffer.clear buffer;

           Log.debug (fun l -> l ~header:"populate" "Retrieve a new Git object (length: %d)." (Cstruct.len raw));

           Store.write_inflated git ~kind:(k2k kind) raw >|= fun hash ->
           Log.debug (fun l -> l ~header:"populate" "Add the object %a to the Git repository from the PACK file."
                         Store.Hash.pp hash);
           Some hash
         | PACKDecoder.Hunk hunks_descr ->
           let hunks = option_map List.rev hunks |> option_default [] in
           let inflated = Cstruct.create (hunks_descr.PACKDecoder.H.target_length) in
           let hash_source = match hunks_descr.PACKDecoder.H.reference with
             | PACKDecoder.H.Hash hash -> hash
             | PACKDecoder.H.Offset off ->
               let off = Int64.sub (PACKDecoder.offset state) off in
               Revidx.find off revidx

           (* XXX(dinosaure): This is come from an assumption about
              the PACK file. Any hunk object has source as an external
              Git object of the PACK file or a previous Git object.

              So [Revidx.find] should never fail. *)
           in

           Log.debug (fun l -> l ~header:"populate" "Catch a Hunk object which has as source the Git object: %a."
                         Store.Hash.pp hash_source);

           Store.read_inflated git hash_source >>= function
           | None ->
             Log.warn (fun l -> l ~header:"populate" "The source Git object %a does not exist yet." Store.Hash.pp hash_source);
             Queue.push (hunks_descr, hunks, cstruct_copy (Buffer.unsafe_contents buffer_hunks), inflated, hash_source) queue;
             Buffer.clear buffer_hunks;

             Lwt.return None
           | Some (kind, raw) ->
             Log.debug (fun l -> l ~header:"populate" "Retrieving the source Git object %a." Store.Hash.pp hash_source);

             match apply hunks_descr hunks (Buffer.unsafe_contents buffer_hunks) raw inflated with
             | Ok result ->
               Buffer.clear buffer_hunks;
               Store.write_inflated git ~kind result >|= fun hash ->
               Log.debug (fun l -> l ~header:"populate" "Add the object %a to the Git repository from the PACK file."
                             Store.Hash.pp hash);
               Some hash
             | Error err ->
               Log.err (fun l -> l ~header:"populate" "Error when we apply the source Git object %a with the Hunk object: %a."
                           Store.Hash.pp hash_source
                           (Fmt.hvbox (Fmt.list ~sep:(Fmt.const Fmt.string ";@ ") pp_optimized_hunk)) hunks);
               Lwt.fail (Failure err))
        >>= fun hash ->
        let revidx = option_map_default (fun hash -> Revidx.add (PACKDecoder.offset state) hash revidx) revidx hash in
        go ~revidx ~src (PACKDecoder.next_object state)
    in

    let rec gogo () = match Queue.pop queue with
      | exception Queue.Empty -> Lwt.return (Ok ())
      | (hunks_descr, hunks, buffer_hunks, inflated, hash_source) ->
        Store.read_inflated git hash_source >>= function
        | None ->
          Queue.push (hunks_descr, hunks, buffer_hunks, inflated, hash_source) queue;
          gogo ()
        | Some (kind, raw) ->
          match apply hunks_descr hunks buffer_hunks raw inflated with
          | Ok result -> Store.write_inflated git ~kind result >>= fun _ -> gogo ()
          | Error err -> Lwt.fail (Failure err)
    in

    let ztmp = Cstruct.create 0x800 in
    let window = PACKDecoder.Inflate.window () in

    go ~revidx:Revidx.empty (PACKDecoder.default ztmp window)
    >>= function
    | Error _ as err -> Lwt.return err
    | Ok () ->
      Queue.fold (fun acc x -> x :: acc) [] queue
      |> Lwt_list.fold_left_s (fun acc (_, _, _, _, hash) -> Store.exists git hash >|= function
        | true -> acc + 1
        | false -> acc) 0
      >>= function
      | 0 ->
        if Queue.is_empty queue
        then Lwt.return (Ok ())
        else Lwt.return (Error `Unresolved_object)
      | _ ->
        Log.debug (fun l -> l ~header:"populate" "Resolving the rest of the PACK file.");
        gogo ()

  let clone git ?headers ?(port = 80) ?(reference = Store.Reference.head) host path =
    let open Lwt.Infix in

    let uri =
      Uri.empty
      |> (fun uri -> Uri.with_scheme uri (Some "http"))
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

        Lwt_result.(populate git stream >>= fun () -> Lwt.return (Ok expect))
      | Error err ->
        Log.err (fun l -> l ~header:"clone" "The HTTP decoder returns an error: %a." Decoder.pp_error err);
        Lwt.return (Error (`Decoder err))
(*

      let body, consumer = Lwt_stream.create () in

      let go req =
        let `Stream producer = Web.Request.body req in
        producer (function
            | Some (buf, off, len) ->
              consumer (Some (buf, off, len))
            | None -> consumer None)
      in

      go req >>= fun () ->

      Log.debug (fun l -> l ~header:"clone" "Launch the POST request to %a."
                    Uri.pp_hum (Web.Request.uri req
                                |> (fun uri -> Uri.with_scheme uri (Some "http"))
                                |> (fun uri -> Uri.with_host uri (Some host))
                                |> (fun uri -> Uri.with_port uri (Some port))));

      >>= fun resp ->
      Decoder.decode resp Decoder.nak >>= function
      | Error err ->
      | Ok first ->
        let stream =
          let first =
            let buffer = Cstruct.of_string first in

            Lwt_stream.of_list [ buffer, 0, String.length first ]
          in

          Lwt_stream.(append first (from (Web.Response.body resp)))
        in

        Lwt.catch Lwt_result.(fun () -> populate git (fun () ->
          Lwt_stream.get stream) >>= fun () -> Lwt.return (Ok expect))
          (function Failure err -> Lwt.return (Error (`Apply err)))
*)
end

