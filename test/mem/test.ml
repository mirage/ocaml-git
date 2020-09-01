let reporter ppf =
  let report src level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_metadata header _tags k ppf fmt =
      Format.kfprintf k ppf
        ("%a[%a]: " ^^ fmt ^^ "\n%!")
        Logs_fmt.pp_header (level, header)
        Fmt.(styled `Magenta string)
        (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_metadata header tags k ppf fmt
  in
  { Logs.report }

let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter (reporter Fmt.stderr)
let () = Logs.set_level ~all:true (Some Logs.Debug)

external random_seed : unit -> int array = "caml_sys_random_seed"

let seed = "4EygbdYh+v35vvrmD9YYP4byT5E3H7lTeXJiIj+dQnc="
let seed = Base64.decode_exn seed

let seed =
  let res = Array.make (String.length seed / 2) 0 in
  for i = 0 to (String.length seed / 2) - 1 do
    res.(i) <- (Char.code seed.[i * 2] lsl 8) lor Char.code seed.[(i * 2) + 1]
  done;
  res

let () =
  let random_seed = seed in
  Fmt.pr "Random: %a.\n%!" Fmt.(Dump.array int) random_seed;
  Random.full_init random_seed

module Store = Git.Mem.Make (Digestif.SHA1)
module Test = Test_store.Make (Digestif.SHA1) (Store)
open Cmdliner

let store =
  let parser x =
    match Fpath.of_string x with
    | Ok v when Sys.is_directory (Fpath.to_string v) ->
        Lwt_main.run
          Lwt.Infix.(
            Store.v v
            >|= Rresult.R.reword_error (Rresult.R.msgf "%a" Store.pp_error))
    | Ok v -> Rresult.R.error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err
  in
  let pp ppf store = Fpath.pp ppf (Store.root store) in
  Arg.conv (parser, pp)

let random =
  let v = Fpath.v (Fmt.strf "git-%06x" (Random.bits () land 0xffffff)) in
  match Lwt_main.run (Store.v v) with
  | Ok v -> v
  | Error err -> Fmt.failwith "empty store: %a" Store.pp_error err

let store =
  let doc = "A git repository." in
  Arg.(value & opt store random & info [ "git" ] ~doc)

let run = Test.test store
let () = Lwt_main.run run
