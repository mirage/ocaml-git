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

open Lwt.Infix
open Result

module Log = struct
  let src = Logs.Src.create "git.mirage" ~doc:"logs git's mirage events"
  include (val Logs.src_log src : Logs.LOG)
end

module type FS = sig
  include Mirage_fs_lwt.S with type page_aligned_buffer = Cstruct.t
  val connect: unit -> t Lwt.t
end

module Lock = struct

  module H = Hashtbl.Make(struct
      type t = string
      let equal = String.equal
      let hash = Hashtbl.hash
    end)

  let locks = H.create 24

  let file_lock file =
    try H.find locks file
    with Not_found ->
      let l = Lwt_mutex.create () in
      H.add locks file l;
      l

  let with_lock l f =
    match l with
    | None   -> f ()
    | Some l -> Lwt_mutex.with_lock l f

end

module FS (FS: FS) (D: Git.Hash.DIGEST) (I: Git.Inflate.S) = struct

  let get_exn pp_exn = function
    | Ok x    -> Lwt.return x
    | Error e -> Lwt.fail_with @@ Fmt.strf "%a" pp_exn e

  let get_fs_error x = get_exn FS.pp_error x
  let get_fs_write_error x = get_exn FS.pp_write_error x

  module IO = struct

    open Mirage_fs

    type path = string
    type lock = Lwt_mutex.t
    let lock_file = Lock.file_lock

    let file_exists t f =
      Log.debug (fun l -> l "file_exists %s" f);
      (* FIXME: it seems that mirage-fs-unix can also raise Sys_error,
         see https://github.com/mirage/mirage-fs-unix/issues/37 *)
      Lwt.catch (fun () ->
          FS.stat t f >|= function
          | Ok _    -> true
          | Error _ -> false
        ) (function
          | Sys_error _ -> Lwt.return_false
          | e -> Lwt.fail e)

    let is_directory t dir =
      Log.debug (fun l -> l "is_directory %s" dir);
      FS.stat t dir >>= get_fs_error >|= fun s ->
      s.directory

    let parent_dir = function
      | "/"
      | "." -> None
      | s   -> Some (Filename.dirname s)

    let mkdir_pool = Lwt_pool.create 1 (fun () -> Lwt.return_unit)

    let mkdir t dirname =
      Log.debug (fun l -> l "mkdir %s" dirname);
      let rec aux dir =
        file_exists t dir >>= function
        | true  -> Lwt.return_unit
        | false ->
          match parent_dir dir with
          | None   -> Lwt.return_unit
          | Some d ->
            aux d >>= fun () ->
            FS.mkdir t dir >>=
            get_fs_write_error
      in
      Lwt_pool.use mkdir_pool (fun () -> aux dirname)

    let list_files t kind dir =
      Log.debug (fun l -> l "list_files %s" dir);
      file_exists t dir >>= function
      | true ->
        FS.listdir t dir >>= get_fs_error >>= fun l ->
        let l = List.filter (fun s -> s <> "." && s <> "..") l in
        let l = List.map (Filename.concat dir) l in
        Lwt_list.filter_s kind l
      | false ->
        Lwt.return_nil

    let directories t dir =
      Log.debug (fun l -> l "directories %s" dir);
      list_files t (fun f -> Lwt.catch
                       (fun () -> is_directory t f)
                       (fun _ -> Lwt.return false)
                   ) dir

    let files t dir =
      Log.debug (fun l -> l "files %s" dir);
      list_files t (fun f -> Lwt.catch
                       (fun () -> is_directory t f >>= fun b -> Lwt.return (not b))
                       (fun _ -> Lwt.return false)
                   ) dir

    let read_file t file =
      Log.debug (fun l -> l "read_file %s" file);
      FS.stat t file >>= function
      | Error _ -> Lwt.return_none
      | Ok s    ->
        is_directory t file >>= function
        | true -> Lwt.fail (Failure (Printf.sprintf "%s is a directory" file))
        | false ->
          FS.read t file 0 (Int64.to_int s.size) >|= function
          | Error _ -> None
          | Ok bs   -> Some (Cstruct.of_string (Cstruct.copyv bs))

    let write_file ?lock t file b =
      Log.debug (fun l -> l "write_file %s" file);
      mkdir t (Filename.dirname file) >>= fun () ->
      Lock.with_lock lock (fun () ->
          FS.create t file    >>= get_fs_write_error >>= fun () ->
          FS.write t file 0 b >>= get_fs_write_error
        )

    let remove_dir t dir =
      Log.debug (fun l -> l "remove_dir %s" dir);
      FS.destroy t dir >>= get_fs_write_error

    let remove_file ?lock t file =
      Log.debug (fun l -> l "remove_file %s" file);
      Lock.with_lock lock (fun () ->
          FS.destroy t file >>= get_fs_write_error
        )

    let test_and_set_file ~lock t file ~test ~set =
      Log.debug (fun l -> l "test_and_set_file %s" file);
      mkdir t (Filename.dirname file) >>= fun () ->
      Lock.with_lock (Some lock) (fun () ->
          let set () =
            (match set with
             | None   -> remove_file t file
             | Some v -> write_file t file v)
            >|= fun () -> true
          in
          read_file t file >>= fun old ->
          match old, test with
          | None, None -> set ()
          | Some old, Some x when Cstruct.equal old x -> set ()
          | _ -> Lwt.return false
        )

    let connect fn = FS.connect () >>= fn
    let mkdir dir = connect (fun t -> mkdir t dir)
    let file_exists file = connect (fun t -> file_exists t file)
    let directories dir = connect (fun t -> directories t dir)
    let files dir = connect (fun t -> files t dir)
    let read_file file = connect (fun t -> read_file t file)
    let chmod ?lock:_ _file _perm = connect (fun _t -> Lwt.return_unit)
    let remove_dir dir = connect (fun t -> remove_dir t dir)

    let remove_file ?lock file =
      connect (fun t -> remove_file ?lock t file)

    let write_file ?temp_dir:_ ?lock file buf =
      connect (fun t -> write_file t ?lock file buf)

    let test_and_set_file ?temp_dir:_ ~lock file ~test ~set =
      connect (fun t -> test_and_set_file t ~lock file ~test ~set)

    let stat_info _file = failwith "TODO"

  end

  include Git.FS.Make(IO)(D)(I)

end

module IO_helper (Channel: Mirage_channel_lwt.S) = struct

  let write oc s =
    let buf = Cstruct.of_string s in
    Channel.write_buffer oc buf;
    Channel.flush oc >|= function
    | Ok () -> ()
    | Error `Closed ->
      Log.debug (fun l -> l "Discarding write to closed channel")
    | Error e ->
      Log.debug (fun l ->
          l "Ignoring error on write: %a" Channel.pp_write_error e)

  let safe_read ~len ic =
    Channel.read_some ~len ic >|= function
    | Ok `Eof        -> None
    | Ok (`Data buf) -> Some buf
    | Error e ->
      Log.debug (fun l -> l "Got error: %a" Channel.pp_error e);
      None

  let read_all ic =
    let len = 4 * 4096 in
    let return l = Lwt.return (List.rev l) in
    let rec aux acc =
      safe_read ~len ic >>= function
      | None                          -> return acc
      | Some b when Cstruct.len b = 0 -> return acc
      | Some b -> aux (Cstruct.to_string b :: acc)
    in
    aux []

  let read_exactly ic n =
    let res = Bytes.create n in
    let rec aux off =
      if off >= n then Lwt.return off
      else (
        safe_read ~len:(n-off) ic >>= fun buf ->
        match buf with
        | None                          -> Lwt.return off
        | Some b when Cstruct.len b = 0 -> Lwt.return off
        | Some b ->
          let i = Cstruct.len b in
          Cstruct.blit_to_string b 0 res off i;
          aux (off + i)
      ) in
    aux 0 >>= fun m ->
    if n <> m then (
      let err =
        Printf.sprintf "Git_mirage.IO.read_exactly: expecting %d, got %d" n m
      in
      Log.err (fun l -> l "%s" err);
      Lwt.fail (Failure err)
    ) else
      Lwt.return (Bytes.to_string res)

  let flush _ = Lwt.return_unit

end

(* channel with functional constructors. *)
module Fflow = Mirage_flow_lwt.F
module Fchannel = Mirage_channel_lwt.Make(Fflow)

module In_channel = struct
  include Fchannel
  let make ?close input =
    create (Fflow.make ?close ~input ())
end

module Out_channel = struct
  include Fchannel
  let make ?close output =
    create (Fflow.make ?close ~output ())
end

(* handle the git:// connections *)
module Git_protocol = struct

  module Flow = Conduit_mirage.Flow
  module Channel = Mirage_channel_lwt.Make(Flow)
  include IO_helper (Channel)

  let safe_close c =
    Channel.close c >>= function
    | Ok ()
    | Error `Closed -> Lwt.return_unit
    | Error e       ->
      Log.debug (fun l -> l "Ignoring error: %a" Channel.pp_write_error e);
      Lwt.return_unit

  let with_connection (resolver, conduit) uri ?init fn =
    assert (Git.Sync.protocol uri = `Ok `Git);
    Log.debug (fun l -> l "Connecting to %s" (Uri.to_string uri));
    Resolver_lwt.resolve_uri ~uri resolver >>= fun endp ->
    Conduit_mirage.client endp >>= fun client ->
    Conduit_mirage.connect conduit client >>= fun flow ->
    let ic = Channel.create flow in
    let oc = Channel.create flow in
    Lwt.finalize
      (fun () ->
         begin match init with
           | None   -> Lwt.return_unit
           | Some s -> write oc s
         end >>= fun () ->
         fn (ic, oc))
      (fun () ->
         safe_close ic >>= fun () ->
         safe_close oc >>= fun () ->
         Lwt.return_unit)

end

(* handle the http(s):// connections *)
module Smart_HTTP = struct

  module Conduit_channel = Mirage_channel_lwt.Make(Conduit_mirage.Flow)
  module HTTP_IO = Cohttp_mirage_io.Make(Conduit_channel)
  module Net = struct
    type ctx = { resolver: Resolver_lwt.t; conduit: Conduit_mirage.t; }
    let default_ctx = {
      resolver = Resolver_mirage.localhost;
      conduit = Conduit_mirage.empty;
    }
    let connect_uri ~ctx uri =
      Resolver_lwt.resolve_uri ~uri ctx.resolver >>= fun endp ->
      Conduit_mirage.client endp >>= fun client ->
      Conduit_mirage.connect ctx.conduit client >>= fun flow ->
      let ch = Conduit_channel.create flow in
      Lwt.return (flow, ch, ch)
    let close_in _ = ()
    let close _ oc = Lwt.async (fun () -> Conduit_channel.close oc)
  end
  module HTTP = struct
    module IO = HTTP_IO
    let close_in = Net.close_in
    let close_out oc = Net.close () oc
  end

  type ctx = Net.ctx

  include IO_helper(Fchannel)

  let safe_close ic =
    Conduit_channel.close ic >|= function
    | Ok ()
    | Error `Closed -> ()
    | Error e       ->
      Log.debug (fun l ->
          l "Ignoring error: %a" Conduit_channel.pp_write_error e)

  module HTTP_fn = Git_http.Flow(HTTP)(In_channel)(Out_channel)
  let with_conduit ctx ?init uri fn =
    Net.connect_uri ~ctx uri >>= fun (_, ic, oc) ->
    Lwt.finalize
      (fun () ->
         begin match init with
           | None   -> Lwt.return_unit
           | Some s -> HTTP_IO.write oc s
         end >>= fun () ->
         fn (ic, oc))
      (fun () -> safe_close ic)

  let with_connection (ctx:ctx) (uri:Uri.t) ?init fn =
    assert (Git.Sync.protocol uri =`Ok `Smart_HTTP);
    HTTP_fn.with_http ?init (with_conduit ctx ?init:None) uri fn

  module Flow = Fflow
  module Channel = Fchannel

end

module IO = struct

  module G = Git_protocol
  module H = Smart_HTTP

  type ctx = Resolver_lwt.t * Conduit_mirage.t

  module Flow = struct
    type 'a io = 'a Lwt.t
    type buffer = Cstruct.t
    module G = Mirage_flow_lwt.Concrete(G.Flow)
    module H = Mirage_flow_lwt.Concrete(H.Flow)
    type flow = [`Git of G.flow | `HTTP of H.flow ]
    type error = G.error (* = H.error *)
    type write_error = G.write_error (* = H.write_error *)
    let pp_error = G.pp_error
    let pp_write_error = G.pp_write_error
    let read = function
      | `Git g -> G.read g
      | `HTTP h -> H.read h
    let write t v = match t with
      | `Git g -> G.write g v
      | `HTTP h ->H.write h v
    let writev t v = match t with
      | `Git g -> G.writev g v
      | `HTTP h -> H.writev h v
    let close = function
      | `Git g -> G.close g
      | `HTTP h -> H.close h
  end

  module Channel = Mirage_channel_lwt.Make(Flow)
  include IO_helper(Channel)
  type ic = Channel.t
  type oc = Channel.t

  let with_connection ?ctx uri ?init fn =
    let resolver, conduit = match ctx with
      | Some x -> x
      | None   ->
        let { H.Net.resolver; conduit } = H.Net.default_ctx in
        resolver, conduit
    in
    match Git.Sync.protocol uri with
    | `Ok `SSH -> failwith "GIT+SSH is not supported with Mirage"
    | `Ok `Git ->
      let fn (ic, oc) =
        let ic = `Git (G.Channel.to_flow ic) in
        let oc = `Git (G.Channel.to_flow oc) in
        fn (Channel.create ic, Channel.create oc)
      in
      G.with_connection (resolver, conduit) ?init uri fn
    | `Ok `Smart_HTTP ->
      let ctx = { H.Net.resolver; conduit } in
      let fn (ic, oc) =
        let ic = `HTTP (H.Channel.to_flow ic) in
        let oc = `HTTP (H.Channel.to_flow oc) in
        fn (Channel.create ic, Channel.create oc)
      in
      H.with_connection ctx ?init:None uri fn
    | `Not_supported x ->
      Lwt.fail (Failure ("Scheme " ^ x ^ " not supported yet"))
    | `Unknown ->
      Lwt.fail (Failure ("Unknown protocol. Must supply a scheme like git://"))

end

module Sync = struct
  module IO = IO
  module Result = Git.Sync.Result
  module Make = Git.Sync.Make(IO)
end

module SHA1_slow = struct

  (* (from uuidm) *)
  (* sha-1 digest. Based on pseudo-code of RFC 3174. Slow and ugly but
     does the job. *)
  let digest ~length ~blit s =
    let set m n c = Bytes.set m n (Char.unsafe_chr c) in
    let sha_1_pad s =
      let len = length s in
      let blen = 8 * len in
      let rem = len mod 64 in
      let mlen = if rem > 55 then len + 128 - rem else len + 64 - rem in
      let m = Bytes.create mlen in
      blit s 0 m 0 len;
      Bytes.fill m len (mlen - len) '\x00';
      Bytes.set m len '\x80';
      if Sys.word_size > 32 then begin
        set m (mlen - 8) (blen lsr 56 land 0xFF);
        set m (mlen - 7) (blen lsr 48 land 0xFF);
        set m (mlen - 6) (blen lsr 40 land 0xFF);
        set m (mlen - 5) (blen lsr 32 land 0xFF);
      end;
      set m (mlen - 4) (blen lsr 24 land 0xFF);
      set m (mlen - 3) (blen lsr 16 land 0xFF);
      set m (mlen - 2) (blen lsr 8 land 0xFF);
      set m (mlen - 1) (blen land 0xFF);
      m
    in
    (* Operations on int32 *)
    let ( &&& ) = ( land ) in
    let ( lor ) = Int32.logor in
    let ( lxor ) = Int32.logxor in
    let ( land ) = Int32.logand in
    let ( ++ ) = Int32.add in
    let lnot = Int32.lognot in
    let sr = Int32.shift_right in
    let sl = Int32.shift_left in
    let cls n x = (sl x n) lor (Int32.shift_right_logical x (32 - n)) in
    (* Start *)
    let m = sha_1_pad s in
    let w = Array.make 16 0l in
    let h0 = ref 0x67452301l in
    let h1 = ref 0xEFCDAB89l in
    let h2 = ref 0x98BADCFEl in
    let h3 = ref 0x10325476l in
    let h4 = ref 0xC3D2E1F0l in
    let a = ref 0l in
    let b = ref 0l in
    let c = ref 0l in
    let d = ref 0l in
    let e = ref 0l in
    for i = 0 to ((Bytes.length m) / 64) - 1 do             (* For each block *)
      (* Fill w *)
      let base = i * 64 in
      for j = 0 to 15 do
        let k = base + (j * 4) in
        w.(j) <- sl (Int32.of_int (Char.code @@ Bytes.get m k)) 24 lor
                 sl (Int32.of_int (Char.code @@ Bytes.get m (k + 1))) 16 lor
                 sl (Int32.of_int (Char.code @@ Bytes.get m (k + 2))) 8 lor
                 (Int32.of_int (Char.code @@ Bytes.get m (k + 3)))
      done;
      (* Loop *)
      a := !h0; b := !h1; c := !h2; d := !h3; e := !h4;
      for t = 0 to 79 do
        let f, k =
          if t <= 19 then (!b land !c) lor ((lnot !b) land !d), 0x5A827999l else
          if t <= 39 then !b lxor !c lxor !d, 0x6ED9EBA1l else
          if t <= 59 then
            (!b land !c) lor (!b land !d) lor (!c land !d), 0x8F1BBCDCl
          else
            !b lxor !c lxor !d, 0xCA62C1D6l
        in
        let s = t &&& 0xF in
        if (t >= 16) then begin
          w.(s) <- cls 1 begin
              w.((s + 13) &&& 0xF) lxor
              w.((s + 8) &&& 0xF) lxor
              w.((s + 2) &&& 0xF) lxor
              w.(s)
            end
        end;
        let temp = (cls 5 !a) ++ f ++ !e ++ w.(s) ++ k in
        e := !d;
        d := !c;
        c := cls 30 !b;
        b := !a;
        a := temp;
      done;
      (* Update *)
      h0 := !h0 ++ !a;
      h1 := !h1 ++ !b;
      h2 := !h2 ++ !c;
      h3 := !h3 ++ !d;
      h4 := !h4 ++ !e
    done;
    let h = Bytes.create 20 in
    let i2s h k i =
      set h k       ((Int32.to_int (sr i 24)) &&& 0xFF);
      set h (k + 1) ((Int32.to_int (sr i 16)) &&& 0xFF);
      set h (k + 2) ((Int32.to_int (sr i 8)) &&& 0xFF);
      set h (k + 3) ((Int32.to_int i) &&& 0xFF);
    in
    i2s h 0 !h0;
    i2s h 4 !h1;
    i2s h 8 !h2;
    i2s h 12 !h3;
    i2s h 16 !h4;
    Git.Hash.of_raw (Bytes.to_string h)

  let string = digest ~length:String.length ~blit:String.blit
  let cstruct = digest ~length:Cstruct.len ~blit:Cstruct.blit_to_string

  let length = 20

end
