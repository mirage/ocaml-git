open Rresult
open Lwt.Infix

let ( >>? ) = Lwt_result.bind

module Make
    (Console : Mirage_console.S)
    (Stack : Mirage_stack.V4V6)
    (Store : Git.S)
    (_ : sig end) =
struct
  module Sync = Git.Mem.Sync (Store)
  module Search = Git.Search.Make (Store.Hash) (Store)

  let main = lazy (Git.Reference.v (Key_gen.branch ()))

  let getline queue =
    let exists ~predicate queue =
      let pos = ref 0 and res = ref (-1) in
      Ke.Rke.iter (fun chr -> if predicate chr && !res = -1 then res := !pos ; incr pos) queue ;
      if !res = -1 then None else Some !res in
    let blit src src_off dst dst_off len =
      Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len in
    match exists ~predicate:((=) '\n') queue with
    | Some pos ->
      let tmp = Bytes.create pos in
      Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off:0 ~len:pos tmp ;
      Ke.Rke.N.shift_exn queue (pos + 1) ;
      Some (Bytes.unsafe_to_string tmp)
    | None -> None

  let getline queue flow =
    let blit src src_off dst dst_off len =
      let src = Cstruct.to_bigarray src in
      Bigstringaf.blit src ~src_off dst ~dst_off ~len in
    let rec go () = match getline queue with
      | Some line ->
        if String.length line > 0 && line.[String.length line - 1] = '\r'
        then Lwt.return_ok (`Line (String.sub line 0 (String.length line - 1)))
        else Lwt.return_ok (`Line line)
      | None ->
        Stack.TCP.read flow >>= function
        | Error err -> Lwt.return_error (`Msg (Fmt.str "%a" Stack.TCP.pp_error err))
        | Ok `Eof -> Lwt.return_ok `Close
        | Ok (`Data tmp) ->
          let len = Cstruct.length tmp in
          Ke.Rke.N.push queue ~blit ~length:Cstruct.length ~off:0 ~len tmp ;
          go () in
    go ()

  let write flow str =
    let cs = Cstruct.of_string str in
    Stack.TCP.write flow cs >|= R.reword_error (R.msgf "%a" Stack.TCP.pp_write_error)

  let writef flow fmt = Format.kasprintf (write flow) fmt
  let writel flow fmt = Format.kasprintf (write flow) (fmt ^^ "\r\n")

  let extend git commit segs entry =
    let open Store in
    Search.find git commit (`Commit (`Path segs)) >>= function
    | None ->
      let singleton = Value.Tree.v [ entry ] in
      write git (Value.tree singleton) >>? fun (hash, _) ->
      Lwt.return_ok hash
    | Some hash ->
      read git hash >>? function
      | Git.Value.Tree tree ->
        let { Git.Tree.name; _ } = entry in
        if List.exists (fun { Git.Tree.name= name'; _ } -> name = name') (Value.Tree.to_list tree)
        then
          let tree = Value.Tree.to_list tree in
          let tree = List.fold_left (fun a x -> if x.Git.Tree.name = name then a else x :: a) [] tree in
          let tree = Value.Tree.v (entry :: tree) in
          write git (Value.tree tree) >>? fun (hash, _) ->
          Lwt.return_ok hash
        else
          let tree = Value.Tree.v (entry :: Value.Tree.to_list tree) in
          write git (Value.tree tree) >>? fun (hash, _) ->
          Lwt.return_ok hash
      | Git.Value.Blob _ ->
        let singleton = Value.Tree.v [ entry ] in
        write git (Value.tree singleton) >>? fun (hash, _) ->
        Lwt.return_ok hash
      | _ -> assert false

  let author () =
    { Git.User.name= "Romain Calascibetta"
    ; email= "romain.calascibetta@gmail.com"
    ; date=
      let ptime = Ptime.unsafe_of_d_ps (Pclock.now_d_ps ()) in
      let tz = match Pclock.current_tz_offset_s () with
        | Some s ->
          let sign = if s < 0 then `Minus else `Plus in
          let hours = s / 3600 in
          let minutes = (s mod 3600) / 60 in
          Some { Git.User.sign; hours; minutes; }
        | None -> None in
      Int64.of_float (Ptime.to_float_s ptime), tz }

  let insert git flow str = match Astring.String.cut ~sep:":" str with
    | None -> writel flow "Invalid insert command."
    | Some (k, v) -> match Fpath.of_string k >>| Fpath.segs with
      | Error _ -> writel flow "Invalid key %S." k
      | Ok [] -> writel flow "Empty key."
      | Ok (_ :: _ as segs) ->
        Store.Ref.resolve git (Lazy.force main) >>? fun root ->
        Store.write git Store.Value.(blob (Blob.of_string v)) >>? fun (hash, _) ->
        let rec go segs entry =
          extend git root segs entry >>? fun hash ->
          match List.rev segs with
          | [] -> Lwt.return_ok hash
          | name :: rsegs ->
            let entry = Git.Tree.entry ~name `Dir hash in
            go (List.rev rsegs) entry in
        let name, segs = let rsegs = List.rev segs in
          List.hd rsegs, List.rev (List.tl rsegs) in
        go segs (Git.Tree.entry ~name `Normal hash) >>? fun tree ->
        let author = author () in
        let commit = Store.Value.Commit.make
          ~tree ~author ~committer:author (Some ".") in
        Store.write git (Store.Value.commit commit) >>? fun (hash, _) ->
        Store.Ref.write git (Lazy.force main) (Git.Reference.uid hash)

  let pp_type ppf = function
    | `Commit -> Format.fprintf ppf "commit"
    | `Blob -> Format.fprintf ppf "blob"
    | `Tag -> Format.fprintf ppf "tag"
    | `Tree -> Format.fprintf ppf "tree"

  let lookup git flow k =
    match Fpath.of_string k with
    | Error _ ->
      writel flow "Invalid key %S." k
    | Ok k ->
      Store.Ref.resolve git (Lazy.force main) >|= R.reword_error (R.msgf "%a" Store.pp_error) >>? fun hash ->
      Search.find git hash (`Commit (`Path (Fpath.segs k))) >>= function
      | None -> writel flow "%a not found." Fpath.pp k
      | Some hash -> Store.read_inflated git hash >>= function
        | None -> writel flow "%a not found." Fpath.pp k
        | Some (k, cs) ->
          writel flow "type: %a." pp_type k >>? fun () ->
          writel flow "@[<hov>%a@]" (Hxd_string.pp Hxd.default) (Cstruct.to_string cs)

  let capabilities =
    [ `Side_band_64k; `Multi_ack_detailed; `Ofs_delta; `Thin_pack; `Report_status ]

  let sync edn ctx git flow =
    Sync.fetch ~capabilities ~ctx edn git ~deepen:(`Depth 1) `All >>= function
    | Ok (Some (pack, _)) -> writel flow "Synchronized with the PACK file: %a." Store.Hash.pp pack
    | Ok None -> writel flow "Already up to date."
    | Error err -> writel flow "Got an error while fetching: %a." Sync.pp_error err

  let push edn ctx git flow =
    Sync.push ~capabilities ~ctx edn git
      [ `Update (Lazy.force main, Lazy.force main) ] >>= function
    | Ok () -> writel flow "Contents pushed."
    | Error err -> writel flow "Got an error while pushing: %a." Sync.pp_error err

  let callback edn ctx git flow =
    let rec go queue flow =
      getline queue flow >>? function
      | `Close -> Stack.TCP.close flow >>= fun () -> Lwt.return_ok ()
      | `Line line -> match line, Astring.String.cut ~sep:" " line with
        | _, Some ("insert", v) ->
          insert git flow v 
          >|= R.reword_error (R.msgf "%a" Store.pp_error)
          >>? fun () -> go queue flow
        | _, Some ("lookup", k) ->
          lookup git flow k >>? fun () -> go queue flow
        | "sync", None ->
          sync edn ctx git flow >>? fun () -> go queue flow
        | "push", None ->
          push edn ctx git flow >>? fun () -> go queue flow
        | "quit", None ->
          writel flow "Bye!" >>? fun () ->
          Stack.TCP.close flow >>= fun () -> Lwt.return_ok ()
        | cmd, _ ->
          writef flow "Invalid command: %S.\n" cmd >>? fun () ->
          go queue flow in
    let queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char in
    go queue flow

  let log console fmt = Format.kasprintf (Console.log console) fmt

  let callback console edn ctx git flow =
    let ipaddr, port = Stack.TCP.dst flow in
    callback edn ctx git flow >>= function
    | Ok () -> Lwt.return_unit
    | Error (`Msg err) ->
      Stack.TCP.close flow >>= fun () ->
      log console "Got an error from %a:%d: %s." Ipaddr.pp ipaddr port err

  let server console stack edn ctx git =
    Stack.listen_tcp stack ~port:(Key_gen.port ()) (callback console edn ctx git) ;
    Stack.listen stack

  let failwith pp = function
    | Ok _ as v -> Lwt.return v
    | Error err -> Fmt.failwith "%a" pp err

  let start console stack git ctx =
    let edn =
      match Smart_git.Endpoint.of_string (Key_gen.remote ()) with
      | Ok edn -> edn
      | Error (`Msg err) -> Fmt.failwith "%s" err in
    Sync.fetch ~capabilities ~ctx edn git ~deepen:(`Depth 1) `All
    >>= failwith Sync.pp_error >>= fun _ -> server console stack edn ctx git
end
