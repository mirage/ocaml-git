let () = Printexc.record_backtrace true
let () = Random.self_init ()

module Capabilities =
struct
  let default =
  [ `Multi_ack
  ; `Thin_pack
  ; `Ofs_delta
  ; `Agent "git/1.9.5"
  ; `Report_status ]
end

let pp_list pp_data ?(sep = fun fmt () -> ()) fmt lst =
  let rec aux = function
    | [] -> ()
    | [ x ] -> pp_data fmt x
    | x :: r -> pp_data fmt x; sep fmt (); aux r
  in

  aux lst

open Lwt.Infix

module PNet =
struct
  type socket = Lwt_unix.file_descr

  open Lwt.Infix

  let read = Lwt_unix.read
  let write = Lwt_unix.write
  let close = Lwt_unix.close
  let socket host port =
    let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.gethostbyname host >>= fun he ->
    Lwt_unix.connect socket (Unix.ADDR_INET (he.Unix.h_addr_list.(0), port)) >|= fun () -> socket
end

module SNet =
struct
  type socket =
    { i : Lwt_io.output_channel
    ; o : Lwt_io.input_channel }

  let read { o; _ } = Lwt_io.read_into o
  let write { i; _ } = Lwt_io.write_from i
  let close { i; o; } =
    let open Lwt.Infix in
    Lwt_io.close i >>= fun () -> Lwt_io.close o
  let socket host port = assert false
end

module Store
  = Store.Make(Sha1)(Fpath)(Fs_lwt_unix)(C_inflate)(Ocaml_deflate)
module Sync
  : module type of Sync.Make(SNet)(Store)(Capabilities)
    with module Hash = Store.Hash
  = Sync.Make(SNet)(Store)(Capabilities)
module Negociator
  : module type of Negociator.Make(Store)
  = Negociator.Make(Store)
module Revision
  = Revision.Make(Store)


let head t =
  Store.Ref.list t >>= function
  | Ok lst ->
    Lwt.try_bind
      (fun () -> Lwt_list.find_s
          (fun (refname, hash) -> Store.Reference.is_head refname |> Lwt.return)
          lst)
      (fun (refname, hash) -> Lwt.return (Ok hash))
      (fun _ -> Lwt.return (Error `Not_found))
  | Error _ as e -> Lwt.return e

let commit t hash =
  Store.read t hash >|= function
  | Ok (Store.Value.Commit commit) -> Ok commit
  | Ok v ->
    let msg = Format.asprintf "expected commit from %a but have %a"
        Store.Hash.pp hash Store.Value.pp v in
    Error (`Invalid_value msg)
  | Error (#Store.error as err) -> Error err

let load_context () =
  Fpath.of_string Sys.argv.(1)
  |> function
  | Ok root -> Store.create ~root ()
  | Error (`Msg sys_err) -> Lwt.return (Error (`System sys_err))

let notify _ = Lwt.return ()

let want refs =
  Lwt.try_bind
    (fun () ->
       Lwt_list.find_s
         (fun (hash, refname, peeled) -> Lwt.return (refname = "HEAD" && not peeled))
         refs)
    (fun (hash, refname, peeled) -> Lwt.return [ hash ])
    (fun _ -> Lwt.return [])

let push refname git refs =
  let pp_data fmt (hash, refname, peeled) = Format.fprintf fmt "(%a, %s, %b)"
      Store.Hash.pp hash refname peeled in

  Format.printf "Receive: [ @[<hov>%a@] ]\n%!"
    (pp_list ~sep:(fun fmt () -> Format.fprintf fmt ";@ ") pp_data)
    refs;

  Store.Ref.graph git >>= function
  | Error _ -> Lwt.return ([], [])
  | Ok graph ->
    Store.Ref.normalize graph (Store.Reference.Ref Store.Reference.head) >>= function
    | Error _ -> Lwt.return ([], [])
    | Ok nhash ->
      Format.printf "current local hash: %a\n%!" Store.Hash.pp nhash;

      Lwt.try_bind
        (fun () ->
           Lwt_list.find_s
            (fun (hash, refname', peeled) ->
              Lwt.return (refname = refname' && not peeled))
            refs)
        (fun (ohash, refname, peeled) ->
           Format.printf "current remote hash: %a\n%!" Store.Hash.pp ohash;
           Lwt.return ([], [ Sync.Client.Encoder.Update (ohash, nhash, refname) ]))
        (fun _ -> Lwt.return ([], []))

let command { Sync.Client.Encoder.request_command; pathname; host; } =
  let host, port = match host with
    | Some (host, port) ->
      let port = match port with Some port -> port | None -> 22 in
      host, port
    | None -> "localhost", 22
  in
  let port = string_of_int port in
  let endpoint = Format.asprintf "git@%s" host in
  let request_command = match request_command with
    | `UploadPack -> "git-upload-pack"
    | `ReceivePack -> "git-receive-pack"
    | `UploadArchive -> "git-upload-archive"
  in
  ("ssh", [| "ssh"; "-p"; port; endpoint; request_command; pathname; |])

let execute ?env git refname git_proto_request main =
  Lwt_process.with_process
    ?env
    (command git_proto_request) (main git refname git_proto_request)

let pure_main refname git =
  Sync.push git ~push:(push refname) ~packer:(Sync.packer ~window:10 ~depth:50) "localhost" "/"

let main git refname git_proto_request process =
  let socket =
    { SNet.i = process#stdin
    ; SNet.o = process#stdout }
  in
  let ctx, state = Sync.Client.context git_proto_request in
  let t = { Sync.socket
          ; input = Bytes.create 65535
          ; output = Bytes.create 65535
          ; ctx }
  in
  Sync.process t state
  >>= Sync.push_handler git ~push:(push refname) ~packer:(Sync.packer ~window:10 ~depth:50) t

let () =
  Lwt_main.run
  (load_context () >>= function
   | Error err ->
     Format.printf "%a\n%!" Store.pp_error err;
     Lwt.return ()
   | Ok t ->
     let env =
       [| Format.asprintf "%s=%s" "SSH_AGENT_PID" (Sys.getenv "SSH_AGENT_PID")
        ; Format.asprintf "%s=%s" "SSH_AUTH_SOCK" (Sys.getenv "SSH_AUTH_SOCK") |]
     in

     execute ~env t "refs/heads/ngit"
       { Sync.Client.Encoder.request_command = `ReceivePack
       ; pathname = "dinosaure/ocaml-git.git"
       ; host = (Some ("github.com", None)) }
       main >>= function
     | Ok () -> Lwt.return ()
     | Error (`Push err) -> Format.printf "(`Push %s)" err; Lwt.return ()
     | Error (`Pack err) -> Format.printf "(`Pack %a)" Sync.PACKEncoder.pp_error err; Lwt.return ())
