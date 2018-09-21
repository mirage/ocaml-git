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

module Fs = Fs
module Net = Net

type endpoint = Net.endpoint = { uri: Uri.t; headers: Cohttp.Header.t }

let endpoint ?headers uri =
  let headers = match headers with
    | None   -> Cohttp.Header.of_list []
    | Some h -> h
  in
  { headers; uri }

module Endpoint = struct
  type t = endpoint
  let uri t = t.uri
end

let dispatch e f = match Uri.scheme e.uri with
  | Some "git" -> f `Tcp
  | Some ("http" | "https") -> f `Http
  | Some s -> Fmt.invalid_arg "%a: invalid scheme (%s)" Uri.pp_hum e.uri s
  | None   -> Fmt.invalid_arg "%a: missing scheme" Uri.pp_hum e.uri

module Sync (G: Git.S) = struct

  module Tcp  = Git.Tcp.Make (Net)(Endpoint)(G)
  module Http = Http.Make (G)
  module Endpoint = Endpoint

  type error =
    | Tcp of Tcp.error
    | Http of Http.error

  let pp_error ppf = function
    | Tcp x  -> Tcp.pp_error ppf x
    | Http x -> Http.pp_error ppf x

  type command =
    [ `Create of G.Hash.t * Git.Reference.t
    | `Delete of G.Hash.t * Git.Reference.t
    | `Update of G.Hash.t * G.Hash.t * Git.Reference.t ]

  let pp_command = Tcp.pp_command

  let tcp_error x =
    Lwt.map (function
        | Ok _ as x -> x
        | Error e -> Error (Tcp e)
      ) x

  let http_error x =
    Lwt.map (function
        | Ok _ as x -> x
        | Error e -> Error (Http e)
      ) x

  let push t ~push ?capabilities e =
    dispatch e (function
        | `Tcp  -> Tcp.push t ~push ?capabilities e |> tcp_error
        | `Http -> Http.push t ~push ?capabilities e |> http_error)

  let ls t ?capabilities e =
    dispatch e (function
        | `Tcp  -> Tcp.ls t ?capabilities e |> tcp_error
        | `Http -> Http.ls t ?capabilities e |> http_error)

  let fetch t ?shallow ?capabilities ~notify ~negociate ~have ~want ?deepen e =
    dispatch e (function
        | `Tcp ->
          Tcp.fetch t ?shallow ?capabilities ~notify ~negociate ~have ~want
            ?deepen e |> tcp_error
        | `Http ->
          Http.fetch t ?shallow ?capabilities ~notify ~negociate ~have ~want
            ?deepen e |> http_error)

  let clone t ?capabilities ~reference e =
    dispatch e (function
        | `Tcp  -> Tcp.clone t ?capabilities ~reference e |> tcp_error
        | `Http -> Http.clone t ?capabilities ~reference e |> http_error)

  let fetch_some t ?capabilities ~references e =
    dispatch e (function
        | `Tcp  -> Tcp.fetch_some t ?capabilities ~references e |> tcp_error
        | `Http -> Http.fetch_some t ?capabilities ~references e |> http_error)

  let fetch_all t ?capabilities ~references e =
    dispatch e (function
        | `Tcp  -> Tcp.fetch_all t ?capabilities ~references e |> tcp_error
        | `Http -> Http.fetch_all t ?capabilities ~references e |> http_error)

  let fetch_one t ?capabilities ~reference e =
    dispatch e (function
        | `Tcp  -> Tcp.fetch_one t ?capabilities ~reference e |> tcp_error
        | `Http -> Http.fetch_one t ?capabilities ~reference e |> http_error)

  let update_and_create t ?capabilities ~references e =
    dispatch e (function
        | `Tcp  ->
          Tcp.update_and_create t ?capabilities ~references e |> tcp_error
        | `Http ->
          Http.update_and_create t ?capabilities ~references e |> http_error)

end

module Index = Index

module Store = struct
  module I = Git.Inflate
  module D = Git.Deflate
  include Git.Store.Make (Digestif.SHA1) (Fs) (I) (D)

  let v ?dotgit ?compression ?buffer root =
    v ?dotgit ?compression ?buffer () root
end
