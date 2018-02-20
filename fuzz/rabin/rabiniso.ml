open Crowbar
module S = Git_unix.FS
module R = Git.Rabin.Default

let length l =
  List.fold_left
    (fun a -> function
      | Git.Rabin.Copy (_, len) -> a + len
      | Git.Rabin.Insert (_, len) -> a + len)
    0 l

let apply a b c l =
  List.fold_left
    (fun pos -> function
      | Git.Rabin.Copy (off, len) ->
         Cstruct.blit a off c pos len;
         pos + len
      | Git.Rabin.Insert (off, len) ->
         Cstruct.blit b off c pos len;
         pos + len)
    0 l |> fun _ -> ()

let () =
  add_test
    ~name:"rabin"
    [ bytes; bytes; ]
  @@ fun a b ->
      let a = Cstruct.of_string a in
      let b = Cstruct.of_string b in
      let index = R.Index.make a in
      let rabin = R.delta index b in

      let length = length rabin in

      if length <> Cstruct.len b
      then fail "Output length missmatch";

      let c = Cstruct.create length in
      apply a b c rabin;

      check_eq ~pp:Cstruct.hexdump_pp ~eq:Cstruct.equal b c
