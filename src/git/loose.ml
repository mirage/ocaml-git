module type S = sig

  module FS: S.FS
  module Value: Value.S
  include module type of Value

  type error =
    [ `SystemFile of FS.File.error
    | `SystemDirectory of FS.Dir.error
    | `SystemIO of string
    | D.error
    | E.error ]

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  val pp_error : error Fmt.t

  val mem:
    root:Fpath.t ->
    Hash.t -> bool Lwt.t

  val read:
    root:Fpath.t ->
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    Hash.t -> (t, error) result Lwt.t

  val inflate:
    root:Fpath.t ->
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    Hash.t -> (kind * Cstruct.t, error) result Lwt.t

  val inflate_wa:
    root:Fpath.t ->
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    result:Cstruct.t ->
    Hash.t -> (kind * Cstruct.t, error) result Lwt.t

  val list:
    root:Fpath.t ->
    Hash.t list Lwt.t

  val size:
    root:Fpath.t ->
    window:Inflate.window ->
    ztmp:Cstruct.t ->
    dtmp:Cstruct.t ->
    raw:Cstruct.t ->
    Hash.t -> (int64, error) result Lwt.t

  val write:
    root:Fpath.t ->
    ?capacity:int ->
    ?level:int ->
    ztmp:Cstruct.t ->
    raw:Cstruct.t ->
    t -> (Hash.t * int, error) result Lwt.t

  val write_inflated:
    root:Fpath.t ->
    ?level:int ->
    raw:Cstruct.t ->
    kind:kind ->
    Cstruct.t -> (Hash.t, error) result Lwt.t
end

module Make
    (H: S.HASH)
    (FS: S.FS)
    (I: S.INFLATE)
    (D: S.DEFLATE)
  : S with module Hash = H
       and module Inflate = I
       and module Deflate = D
       and module FS = FS
       and module Blob = Blob.Make(H)
       and module Commit = Commit.Make(H)
       and module Tree = Tree.Make(H)
       and module Tag = Tag.Make(H)
       and type t = Value.Make(H)(I)(D).t
= struct
  module Log =
  struct
    let src = Logs.Src.create "git.loose" ~doc:"logs git's loose event"
    include (val Logs.src_log src : Logs.LOG)
  end

  module FS = FS

  module Value = Value.Make(H)(I)(D)
  include Value

  type error =
    [ `SystemFile of FS.File.error
    | `SystemDirectory of FS.Dir.error
    | `SystemIO of string
    | D.error
    | E.error ]

  type kind =
    [ `Commit
    | `Tree
    | `Tag
    | `Blob ]

  let pp_error ppf = function
    | #D.error as err -> D.pp_error ppf err
    | #E.error as err -> E.pp_error ppf err
    | `SystemIO err -> Helper.ppe ~name:"`SystemIO" Fmt.string ppf err
    | `SystemFile sys_err -> Helper.ppe ~name:"`SystemFile" FS.File.pp_error ppf sys_err
    | `SystemDirectory sys_err -> Helper.ppe ~name:"`SystemDirectory" FS.Dir.pp_error ppf sys_err

  let hash_get : Hash.t -> int -> int = fun h i -> Char.code @@ Hash.get h i

  let explode hash =
    Fmt.strf "%02x" (hash_get hash 0),
    let buf = Buffer.create ((Hash.Digest.length - 1) * 2) in
    let ppf = Fmt.with_buffer buf in

    for i = 1 to Hash.Digest.length - 1
    do Fmt.pf ppf "%02x%!" (hash_get hash i) done;

    Buffer.contents buf

  let mem ~root hash =
    let open Lwt.Infix in

    let first, rest = explode hash in
    Log.debug (fun l -> l ~header:"mem" "Checking if the object %a is a loose file (%a)."
                  Hash.pp hash
                  Fpath.pp Fpath.(root / "objects" / first / rest)[@warning "-44"]);

    FS.File.exists Fpath.(root / "objects" / first / rest)[@warning "-44"]
    >>= function Ok v -> Lwt.return v
               | Error _ -> Lwt.return false

  (* XXX(dinosaure): make this function more resilient: if [of_hex]
     fails), avoid the path. *)
  let list ~root =
    let open Lwt.Infix in

    FS.Dir.contents
      ~dotfiles:false
      ~rel:true
      Fpath.(root / "objects")[@warning "-44"]
    >>= function
    | Error sys_err ->
      Log.err (fun l -> l ~header:"list" "Retrieving a file-system error: %a." FS.Dir.pp_error sys_err);
      Lwt.return []
    | Ok firsts ->
      Lwt_list.fold_left_s
        (fun acc first ->
           FS.Dir.contents ~dotfiles:false ~rel:true (Fpath.(append (root / "objects") first)[@warning "-44"])
           >>= function
           | Ok paths ->
             Lwt_list.fold_left_s
               (fun acc path ->
                  try
                    (Hash.of_hex Fpath.((to_string first) ^ (to_string path)))
                    |> fun v -> Lwt.return (v :: acc)
                  with _ ->
                    Log.warn (fun l -> l ~header:"list" "Retrieving a malformed file: %s / %s."
                                 (Fpath.to_string first) (Fpath.to_string path));
                    Lwt.return acc)
               acc
               paths
           | Error _ -> Lwt.return acc)
        [] firsts

  type 't decoder =
    (module S.DECODER
      with type t = 't
       and type init = Inflate.window * Cstruct.t * Cstruct.t
       and type error = [ `Decoder of string
                        | `Inflate of Inflate.error ])

  let gen (type t) ~root ~window ~ztmp ~dtmp ~raw (decoder : t decoder) hash : (t, error) result Lwt.t =
    let module D = (val decoder) in

    let first, rest = explode hash in
    let decoder     = D.default (window, ztmp, dtmp) in

    let open Lwt.Infix in

    Log.debug (fun l -> l ~header:"read" "Reading the loose object %a."
                  Fpath.pp Fpath.(root / "objects" / first / rest)[@warning "-44"]);

    FS.File.open_r ~mode:0o400 Fpath.(root / "objects" / first / rest)[@warning "-44"]
    >>= function
    | Error sys_err ->
      Log.err (fun l -> l ~header:"read" "Retrieving a file-system error: %a." FS.File.pp_error sys_err);
      Lwt.return (Error (`SystemFile sys_err))
    | Ok ic ->
      let rec loop decoder = match D.eval decoder with
        | `Await decoder ->
          FS.File.read raw ic >>=
          (function
            | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
            | Ok n ->
              Log.debug (fun l -> l ~header:"read" "Reading %d byte(s) of the file-descriptor (object: %a)." n Hash.pp hash);

              match D.refill (Cstruct.sub raw 0 n) decoder with
              | Ok decoder -> loop decoder
              | Error (#D.error as err) -> Lwt.return (Error err))
        | `Error (_, (#D.error as err)) -> Lwt.return (Error err)
        | `End (_, value) -> Lwt.return (Ok value)
      in

      Lwt.finalize
        (fun () -> loop decoder)
        (fun () -> FS.File.close ic >>= function
           | Ok () -> Lwt.return ()
           | Error sys_err ->
             Log.err (fun l -> l ~header:"read" "Retrieve an error when we close the file-descriptor: %a."
                         FS.File.pp_error sys_err);
             Lwt.return ())
      >>= fun ret ->
      Log.debug (fun l -> l ~header:"read" "Finish to read the object %s / %s."
                    first rest);
      Lwt.return ret

  let read ~root ~window ~ztmp ~dtmp ~raw hash =
    gen ~root ~window ~ztmp ~dtmp ~raw (module D) hash

  module HeaderAndBody =
  struct
    type t = [ `Commit | `Blob | `Tag | `Tree ] * Cstruct.t

    let kind = Value.A.kind
    let int64 = Value.A.length

    let to_end cs =
      let open Angstrom in
      let pos = ref 0 in

      fix @@ fun m ->
      available >>= function
      | 0 ->
        peek_char
        >>= (function
            | Some _ -> m
            | None ->
              commit *> return (Cstruct.sub cs 0 !pos))
      | n -> take n >>= fun chunk ->
        (* XXX(dinosaure): this code [blit] only what is possible to
           copy to [cs]. It can be happen than we don't store all of
           the git object in [cs] but in specific context (when we
           want to decode a source of a delta-ification), this is what
           we want, store only what is needed and limit the memory
           consumption.

           This code is close to the [~result] argument of [decoder]
           and, in fact, if we don't want to store the git object in a
           specific user defined buffer, we ensure to allocate what is
           needed to store all of the git object. *)
        let n' = min n (Cstruct.len cs - !pos) in
        Cstruct.blit_from_string chunk 0 cs !pos n';
        pos := !pos + n;

        commit *> (if n = 0 then return cs else m)

    let sp = ' '
    let nl = '\000'

    let decoder ~result =
      let open Angstrom in
      kind <* char sp
      >>= fun kind -> int64 <* char nl <* commit
      >>= fun length ->
      (match result with
       | Some result -> to_end result
       | None -> to_end (Cstruct.create (Int64.to_int length)))
      >>| fun cs -> kind, cs
  end

  module I = Helper.MakeInflater(Inflate)(struct include HeaderAndBody let decoder = decoder ~result:None end)

  let inflate ~root ~window ~ztmp ~dtmp ~raw hash =
    gen ~root ~window ~ztmp ~dtmp ~raw (module I) hash

  let inflate_wa ~root ~window ~ztmp ~dtmp ~raw ~result hash =
    let module P = Helper.MakeInflater(Inflate)(struct include HeaderAndBody let decoder = decoder ~result:(Some result) end) in
    gen ~root ~window ~ztmp ~dtmp ~raw (module P) hash

  module HeaderOnly =
  struct
    type t = [ `Commit | `Blob | `Tag | `Tree ] * int64

    let kind = HeaderAndBody.kind
    let int64 = HeaderAndBody.int64
    let decoder =
      let open Angstrom in
      kind <* take 1
      >>= fun kind -> int64 <* advance 1
      >>| fun length -> kind, length
  end

  module S = Helper.MakeInflater(Inflate)(HeaderOnly)

  let size ~root ~window ~ztmp ~dtmp ~raw hash =
    let first, rest = explode hash in
    let decoder     = S.default (window, ztmp, dtmp) in

    let open Lwt.Infix in

    Log.debug (fun l -> l ~header:"size" "Reading the loose object %a."
                  Fpath.pp Fpath.(root / "objects" / first / rest)[@warning "-44"]);

    FS.File.open_r ~mode:0o400 Fpath.(root / "objects" / first / rest)[@warning "-44"]
    >>= function
    | Error sys_err ->
      Log.err (fun l -> l ~header:"size" "Retrieving a file-system error: %a." FS.File.pp_error sys_err);
      Lwt.return (Error (`SystemFile sys_err))
    | Ok read ->
      let rec loop decoder = match S.eval decoder with
        | `Await decoder ->
          FS.File.read raw read >>=
          (function
            | Error sys_err -> Lwt.return (Error (`SystemFile sys_err))
            | Ok n -> match S.refill (Cstruct.sub raw 0 n) decoder with
              | Ok decoder -> loop decoder
              | Error (#S.error as err) -> Lwt.return (Error err))
        | `Error (_, (#S.error as err)) -> Lwt.return (Error err)
        | `End (_, (_, size)) ->
          FS.File.close read >|= function
          | Ok () -> Ok size
          | Error sys_err -> Error (`SystemFile sys_err)
          (* XXX(dinosaure): [gen] checks if we consume all of the
             input. But for this compute, we don't need to compute all.
             It's redundant. *)
      in

      loop decoder

  let write_inflated ~root ?(level = 4) ~raw ~kind value =
    let open Lwt.Infix in

    let header = Cstruct.of_string
        (Fmt.strf "%s %d\000%!"
           (match kind with
            | `Commit -> "commit"
            | `Blob -> "blob"
            | `Tree -> "tree"
            | `Tag -> "tag")
           (Cstruct.len value))
    in

    let digest value' =
      let ctx = Hash.Digest.init () in
      Hash.Digest.feed ctx value';
      Hash.Digest.get ctx
    in

    let value' = Cstruct.concat [ header; value ] in
    let state = Deflate.no_flush 0 (Cstruct.len value') (Deflate.default level) in
    let hash = digest value' in
    let first, rest = explode hash in

    let module E =
    struct
      type state  = Deflate.t
      type raw    = Cstruct.t
      type result = unit
      type error  = Deflate.error

      let raw_length = Cstruct.len
      let raw_blit   = Cstruct.blit

      let rec eval raw state =
        match Deflate.eval ~src:value' ~dst:raw state with
        | `Await state -> eval raw (Deflate.finish state)
        | `Flush state -> Lwt.return (`Flush state)
        | `Error (state, error) -> Lwt.return (`Error (state, error))
        | `End state -> Lwt.return (`End (state, ()))

      let used = Deflate.used_out
      let flush = Deflate.flush
    end in

    FS.Dir.create ~path:true Fpath.(root / "objects" / first)
    >>= function
    | Error err -> Lwt.return (Error (`SystemDirectory err))
    | Ok (true | false) ->
      FS.File.open_w ~mode:0o644 Fpath.(root / "objects" / first / rest)[@warning "-44"] (* XXX(dinosaure): shadowing ( / ). *)
      >>= function
      | Error sys_error -> Lwt.return (Error (`SystemFile sys_error))
      | Ok oc ->
        Lwt.finalize
          (fun () -> Helper.safe_encoder_to_file
              ~limit:50
              (module E)
              FS.File.write
              oc raw state)
          (fun () -> FS.File.close oc >>= function
             | Ok () -> Lwt.return ()
             | Error sys_err ->
               Log.err (fun l -> l ~header:"write_inflated" "Retrieve an error when we close the file-descriptor: %a."
                           FS.File.pp_error sys_err);
               Lwt.return ())
        >>= function
        | Ok () ->
          Lwt.return (Ok hash)
        | Error err ->
          match err with
          | `Stack -> Lwt.return (Error (`SystemIO (Fmt.strf "Impossible to store the loosed Git object %a" (Fmt.hvbox Hash.pp) hash)))
          | `Writer sys_err -> Lwt.return (Error (`SystemFile sys_err))
          | `Encoder err -> Lwt.return (Error (`Deflate err))

  let write ~root ?(capacity = 0x100) ?(level = 4) ~ztmp ~raw value =
    let hash        = digest value in
    let first, rest = explode hash in
    let encoder     = E.default (capacity, value, level, ztmp) in

    let open Lwt.Infix in

    Log.debug (fun l -> l ~header:"write" "Writing a new loose object %a."
                  Fpath.pp Fpath.(root / "objects" / first / rest)[@warning "-44"]);

    let module E =
    struct
      type state  = E.encoder
      type raw    = Cstruct.t
      type result = int
      type error  = E.error

      let raw_length = Cstruct.len
      let raw_blit   = Cstruct.blit

      let eval raw state =
        match E.eval raw state with
        | `Flush state -> Lwt.return (`Flush state)
        | `Error error -> Lwt.return (`Error (state, error))
        | `End state -> Lwt.return (`End state)

      let used = E.used
      let flush = E.flush
    end in

    FS.Dir.create ~path:true Fpath.(root / "objects" / first) >>= function
    | Error err ->
      Log.err (fun l -> l ~header:"write" "Retrieve an error when we try to create the directory %a: %a."
                  Fpath.pp Fpath.(root / "objects" / first)
                  FS.Dir.pp_error err);
      Lwt.return (Error (`SystemDirectory err))
    | Ok (true | false) ->
      FS.File.open_w ~mode:0o644 Fpath.(root / "objects" / first / rest)[@warning "-44"] (* XXX(dinosaure): shadowing ( / ). *)
      >>= function
      | Error sys_error ->
        Log.err (fun l -> l ~header:"write" "Retrieve an error when we open/create the file %a: %a."
                    Fpath.pp Fpath.(root / "objects" / first / rest)
                    FS.File.pp_error sys_error);
        Lwt.return (Error (`SystemFile sys_error))
      | Ok oc ->
        Lwt.finalize
          (fun () ->
             Helper.safe_encoder_to_file
               ~limit:50
               (module E)
               FS.File.write
               oc raw encoder)
          (fun () -> FS.File.close oc >>= function
             | Ok () -> Lwt.return ()
             | Error sys_err ->
               Log.err (fun l -> l ~header:"write" "Retrieve an error when we close the file-descriptor: %a."
                           FS.File.pp_error sys_err);
               Lwt.return ())
        >>= function
        | Ok r ->
          Log.debug (fun l -> l ~header:"write" "Finish to write the object %s / %s." first rest);
          Lwt.return (Ok (hash, r))
        | Error err ->
          match err with
          | `Stack -> Lwt.return (Error (`SystemIO (Fmt.strf "Impossible to store the loosed Git object %a" (Fmt.hvbox Hash.pp) hash)))
          | `Writer sys_err -> Lwt.return (Error (`SystemFile sys_err))
          | `Encoder (`Deflate err) -> Lwt.return (Error (`Deflate err))
end
