module type S =
sig
  module Digest     : Ihash.IDIGEST
  module Path       : Path.S
  module FileSystem : Fs.S
  module Hash       : Common.BASE

  type t = [ `Peeled of Hash.t | `Ref of string * Hash.t ] list

  module A : Common.ANGSTROM with type t = t
  module D : Common.DECODER  with type t = t
                              and type raw = Cstruct.t
                              and type init = Cstruct.t
                              and type error = [ `Decoder of string ]
  module M : Common.MINIENC with type t = t
  module E : Common.ENCODER with type t = t
                             and type raw = Cstruct.t
                             and type init = int * t

  type error = [ FileSystem.File.error
               | D.error ]

  val write : root:Path.t -> ?capacity:int -> raw:Cstruct.t -> t -> (unit, error) result Lwt.t
  val read  : root:Path.t -> dtmp:Cstruct.t -> raw:Cstruct.t -> (t, error) result Lwt.t
end

module Make
    (Digest : Ihash.IDIGEST with type t = Bytes.t
                             and type buffer = Cstruct.t)
    (Path : Path.S)
    (FileSystem : Fs.S with type path = Path.t
                        and type File.error = [ `System of string ]
                        and type File.raw = Cstruct.t)
  : S with type Hash.t = Digest.t
       and module Digest = Digest
       and module Path = Path
       and module FileSystem = FileSystem
= struct
  module Digest = Digest
  module Path = Path
  module FileSystem = FileSystem

  module Hash = Helper.BaseBytes

  type t = [ `Peeled of hash | `Ref of string * hash ] list
  and hash = Hash.t

  let hash_of_hex_string x =
    Helper.BaseBytes.of_hex (Bytes.unsafe_of_string x)

  let hash_to_hex_string x =
    Helper.BaseBytes.to_hex x
    |> Bytes.to_string

  module A =
  struct
    type nonrec t = t

    open Angstrom

    let hash =
      take (Digest.length * 2)
      >>| hash_of_hex_string

    let end_of_line =
      skip_while (function '\r' | '\n' -> false | _ -> true)
      *> peek_char >>= function
      | Some '\n' -> take 1 >>= fun _ -> return ()
      | Some '\r' -> take 2 >>= fun _ -> return ()
      | Some _ -> assert false
      | None -> return ()

    let info =
      (option false (char '^' *> return true)) >>= fun peeled ->
      hash >>= fun hash -> match peeled with
      | true  -> end_of_line *> return (`Peeled hash)
      | false ->
        take_while (function '\x20'
                           | '\x09' -> true
                           | _      -> false)
        *> take_while (function '\000' .. '\039' -> false
                              | '\127'           -> false
                              | '~' | '^'
                              | ':' | '?' | '*'
                              | '['              -> false
                              | _                -> true)
        >>= fun refname ->
        end_of_line >>= fun () ->
        return (`Ref (refname, hash))

    let decoder =
      fix @@ fun m ->
      (peek_char >>= function
        | Some '#' ->
          end_of_line *> m
        | Some chr ->
          info >>= fun x -> m >>= fun r -> return (x :: r)
        | None -> return [])
  end

  module M =
  struct
    type nonrec t = t

    open Minienc

    let write_newline k e =
      if Sys.win32
      then write_string "\r\n" k e
      else write_string "\n" k e

    let write_hash x k e =
      write_string (hash_to_hex_string x) k e

    let write_info x k e = match x with
      | `Peeled hash ->
        (write_char '^'
         @@ write_hash hash k)
        e
      | `Ref (refname, hash) ->
        (write_hash hash
         @@ write_char ' '
         @@ write_string refname k)
        e

    let write_list ?(sep = fun k e -> k e) write_data lst k e =
      let rec aux l e = match l with
        | [] -> k e
        | [ x ] -> write_data x k e
        | x :: r ->
          (write_data x
           @@ sep
           @@ aux r)
          e
      in
      aux lst e

    let encoder l k e =
      (write_string "# pack-refs with: peeled fully-peeled"
       @@ write_newline
       @@ write_list ~sep:write_newline write_info l k)
      e
  end

  module D = Helper.MakeDecoder(A)
  module E = Helper.MakeEncoder(M)

  type error =
    [ FileSystem.File.error
    | D.error ]

  let pp_error ppf = function
    | #FileSystem.File.error as err -> FileSystem.File.pp_error ppf err
    | #D.error as err -> D.pp_error ppf err

  let read ~root ~dtmp ~raw =
    let decoder = D.default dtmp in

    let open Lwt.Infix in

    FileSystem.File.open_r ~mode:0o400 ~lock:(Lwt.return ()) Path.(root / "packed-refs")
    >>= function
    | Error (#FileSystem.File.error as err) -> Lwt.return (Error err)
    | Ok read ->
      let rec loop decoder = match D.eval decoder with
        | `Await decoder ->
          FileSystem.File.read raw read >>=
          (function
            | Error (#FileSystem.File.error as err) -> Lwt.return (Error err)
            | Ok 0 -> loop (D.finish decoder)
            | Ok n -> match D.refill (Cstruct.sub raw 0 n) decoder with
              | Ok decoder -> loop decoder
              | Error (#D.error as err) -> Lwt.return (Error err))
        | `End (rest, value) -> Lwt.return (Ok value)
        | `Error (res, (#D.error as err)) -> Lwt.return (Error err)
      in

      loop decoder

  let write ~root ?(capacity = 0x100) ~raw value =
    let open Lwt.Infix in

    let state = E.default (capacity, value) in

    let module E =
    struct
      type state  = E.encoder
      type raw    = Cstruct.t
      type result = int
      type error  = E.error

      let raw_length = Cstruct.len
      let raw_blit   = Cstruct.blit

      type rest = [ `Flush of state | `End of (state * result) ]

      let eval raw state = match E.eval raw state with
        | `Error err -> Lwt.return (`Error (state, err))
        | #rest as rest -> Lwt.return rest

      let used  = E.used
      let flush = E.flush
    end in

    FileSystem.File.open_w ~mode:0o644 ~lock:(Lwt.return ()) Path.(root / "packed-refs")
    >>= function
    | Error (#FileSystem.File.error as err) -> Lwt.return (Error err)
    | Ok write ->
      Helper.safe_encoder_to_file
        ~limit:50
        (module E)
        FileSystem.File.write
        write raw state
      >>= function
      | Ok _ -> FileSystem.File.close write >>=
        (function
          | Ok () -> Lwt.return (Ok ())
          | Error (#FileSystem.File.error as err) -> Lwt.return (Error err))
      | Error err ->
        FileSystem.File.close write >>= function
        | Error (#FileSystem.File.error as err) -> Lwt.return (Error err)
        | Ok () -> match err with
          | `Stack -> Lwt.return (Error (`System "Impossible to store the packed-refs file"))
          | `Writer (#FileSystem.File.error as err) -> Lwt.return (Error err)
          | `Encoder `Never -> assert false
end
