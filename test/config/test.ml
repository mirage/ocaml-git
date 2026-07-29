(* Key syntax and lookup. The parsing and typed-value cases, whose
   expectations come from git itself, are in [Cases]. *)

module C = Git.Config

let key_t = Alcotest.(result string string)

let key s =
  let r : (C.Key.t, [ `Msg of string ]) result = C.Key.of_string s in
  Result.fold
    ~ok:(fun k -> Ok (C.Key.to_string k))
    ~error:(fun (`Msg m) -> Error m)
    r

let key_case name s expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check key_t name expected (key s))

let keys =
  [
    key_case "plain" "core.bare" (Ok "core.bare");
    (* A section and a variable name are case-insensitive; a subsection is not. *)
    key_case "folds case" "CORE.BARE" (Ok "core.bare");
    key_case "subsection" "remote.origin.url" (Ok "remote.origin.url");
    key_case "subsection keeps case" "remote.Origin.URL"
      (Ok "remote.Origin.url");
    (* The LAST dot separates the name, so a subsection may contain dots. *)
    key_case "dotted subsection" "a.b.c.d" (Ok "a.b.c.d");
    key_case "empty subsection" "a..b" (Ok "a..b");
    key_case "empty section" ".a.b" (Ok ".a.b");
    key_case "no section" "x" (Error "key does not contain a section: x");
    key_case "leading dot only" ".x"
      (Error "key does not contain a section: .x");
    key_case "no name" "a." (Error "key does not contain variable name: a.");
    key_case "name starts with a digit" "a.1b" (Error "invalid key: a.1b");
    key_case "underscore in section" "a_b.c" (Error "invalid key: a_b.c");
    key_case "underscore in name" "a.b_c" (Error "invalid key: a.b_c");
  ]

let built name k expected =
  Alcotest.test_case name `Quick (fun () ->
      let r : (C.Key.t, [ `Msg of string ]) result = k in
      let got =
        Result.fold
          ~ok:(fun k -> Ok (C.Key.to_string k))
          ~error:(fun (`Msg m) -> Error m)
          r
      in
      Alcotest.check key_t name expected got)

let constructed =
  [
    built "v" (C.Key.v "core" "bare") (Ok "core.bare");
    built "v folds case"
      (C.Key.v ~subsection:"Origin" "REMOTE" "URL")
      (Ok "remote.Origin.url");
    built "v rejects an empty section" (C.Key.v "" "x")
      (Error "invalid section \"\"");
    built "v rejects a leading digit" (C.Key.v "a" "1x")
      (Error "invalid variable name \"1x\"");
    built "v rejects a newline in a subsection"
      (C.Key.v ~subsection:"a\nb" "a" "x")
      (Error "a subsection cannot contain a newline");
  ]

let structure =
  [
    Alcotest.test_case "parts" `Quick (fun () ->
        let r : (C.Key.t, [ `Msg of string ]) result =
          C.Key.of_string "a.b.c.d"
        in
        Result.fold
          ~error:(fun (`Msg m) -> Alcotest.failf "a.b.c.d: %s" m)
          ~ok:(fun k ->
            Alcotest.(check string) "section" "a" (C.Key.section k);
            Alcotest.(check (option string))
              "subsection" (Some "b.c") (C.Key.subsection k);
            Alcotest.(check string) "name" "d" (C.Key.name k))
          r);
  ]

let sample =
  {|[core]
	bare = true
	repositoryformatversion = 0
	logallrefupdates
[remote "origin"]
	url = https://example.com/x.git
	fetch = +refs/heads/*:refs/remotes/origin/*
[remote "Upstream"]
	url = https://example.com/y.git
[a]
	x = 1
	x = 2
|}

let with_sample name f =
  Alcotest.test_case name `Quick (fun () ->
      let r : (C.t, [ `Msg of string ]) result = C.of_string sample in
      Result.fold ~error:(fun (`Msg m) -> Alcotest.failf "sample: %s" m) ~ok:f r)

let looked_up t s =
  let r : (C.Key.t, [ `Msg of string ]) result = C.Key.of_string s in
  Result.fold ~ok:(fun k -> C.get t k) ~error:(fun _ -> None) r

let all t s =
  let r : (C.Key.t, [ `Msg of string ]) result = C.Key.of_string s in
  Result.fold ~ok:(fun k -> C.get_all t k) ~error:(fun _ -> []) r

let present t s =
  let r : (C.Key.t, [ `Msg of string ]) result = C.Key.of_string s in
  Result.fold ~ok:(fun k -> C.mem t k) ~error:(fun _ -> false) r

let typed f t s =
  let r : (C.Key.t, [ `Msg of string ]) result = C.Key.of_string s in
  Result.fold
    ~ok:(fun k ->
      let r = f t k in
      Result.fold ~ok:(fun v -> Ok v) ~error:(fun (`Msg m) -> Error m) r)
    ~error:(fun (`Msg m) -> Error m)
    r

let value_o = Alcotest.option Cases.value_t

let lookup =
  [
    with_sample "reads a variable" (fun t ->
        Alcotest.check value_o "url"
          (Some (C.Value "https://example.com/x.git"))
          (looked_up t "remote.origin.url"));
    with_sample "section and name are case-insensitive" (fun t ->
        Alcotest.check value_o "REMOTE.origin.URL"
          (Some (C.Value "https://example.com/x.git"))
          (looked_up t "REMOTE.origin.URL"));
    with_sample "a subsection is case-sensitive" (fun t ->
        Alcotest.check value_o "remote.ORIGIN.url" None
          (looked_up t "remote.ORIGIN.url"));
    with_sample "a subsection keeps its own case" (fun t ->
        Alcotest.check value_o "remote.Upstream.url"
          (Some (C.Value "https://example.com/y.git"))
          (looked_up t "remote.Upstream.url"));
    (* git reports the last setting of a repeated variable. *)
    with_sample "the last setting wins" (fun t ->
        Alcotest.check value_o "a.x" (Some (C.Value "2")) (looked_up t "a.x"));
    with_sample "every setting, in order" (fun t ->
        Alcotest.check
          (Alcotest.list Cases.value_t)
          "a.x"
          [ C.Value "1"; C.Value "2" ]
          (all t "a.x"));
    with_sample "an unset variable" (fun t ->
        Alcotest.check value_o "core.missing" None (looked_up t "core.missing"));
    with_sample "a variable with no value is still set" (fun t ->
        Alcotest.(check bool) "mem" true (present t "core.logallrefupdates");
        Alcotest.check value_o "get" (Some C.Bare)
          (looked_up t "core.logallrefupdates"));
    with_sample "as a boolean" (fun t ->
        Alcotest.(check (result (option bool) string))
          "core.bare" (Ok (Some true))
          (typed C.get_bool t "core.bare"));
    with_sample "a variable with no value is true" (fun t ->
        Alcotest.(check (result (option bool) string))
          "core.logallrefupdates" (Ok (Some true))
          (typed C.get_bool t "core.logallrefupdates"));
    with_sample "as an integer" (fun t ->
        Alcotest.(check (result (option int) string))
          "core.repositoryformatversion" (Ok (Some 0))
          (typed C.get_int t "core.repositoryformatversion"));
    with_sample "an unset variable has no type error" (fun t ->
        Alcotest.(check (result (option int) string))
          "core.missing" (Ok None)
          (typed C.get_int t "core.missing"));
    with_sample "a value that is not a number" (fun t ->
        Alcotest.(check (result (option int) string))
          "remote.origin.url"
          (Error "bad numeric config value \"https://example.com/x.git\"")
          (typed C.get_int t "remote.origin.url"));
  ]

(* The one place we knowingly read more than git reports. git's parser holds
   the whole value too, but it hands values on as C strings, so everything
   downstream of it — `git config --list` included — stops at the first NUL.
   OCaml strings carry their length, so truncating here would lose data for no
   reason. *)
let nul =
  [
    (* git reports a.x=a *)
    Alcotest.test_case "a NUL byte in a value is kept" `Quick (fun () ->
        Alcotest.check Cases.parse_t "kept whole"
          (Ok [ "a.x", C.Value "a\000b" ])
          (Cases.parse "[a]\n\tx = a\000b\n"));
    (* git reports a.b=1, so two subsections differing only past the NUL are
       the same variable to it, and different ones to us. *)
    Alcotest.test_case "a NUL byte in a subsection is kept" `Quick (fun () ->
        Alcotest.check Cases.parse_t "kept whole"
          (Ok [ "a.b\000c.x", C.Value "1" ])
          (Cases.parse "[a \"b\000c\"]\n\tx = 1\n"));
  ]

(* The recorded int cases narrow anything git can hold but an OCaml int cannot
   into "expect an error", so pin that boundary directly rather than leaving it
   implied by the harness. git reads this accessor as an int64; an OCaml int is
   63 bits on a 64-bit host, so the band in between is a number to git and an
   error here. *)
let narrowing =
  let int_of s =
    Result.fold ~ok:(fun n -> Some n) ~error:(fun _ -> None) (C.int (C.Value s))
  in
  [
    Alcotest.test_case "int is narrower than git's" `Quick (fun () ->
        Alcotest.(check (option int))
          "the largest OCaml int is read" (Some max_int)
          (int_of (string_of_int max_int));
        Alcotest.(check (option int))
          "one past it is not, though git reads it" None
          (int_of (Int64.to_string (Int64.add (Int64.of_int max_int) 1L))));
    Alcotest.test_case "the smallest OCaml int is read" `Quick (fun () ->
        Alcotest.(check (option int))
          "min_int" (Some min_int)
          (int_of (string_of_int min_int)));
  ]

let () =
  Alcotest.run "git-config"
    [
      "parsing", Cases.parsing;
      "round-trip", Cases.round_trips;
      "bool", Cases.bools;
      "int", Cases.ints;
      "keys", keys;
      "keys/constructed", constructed;
      "keys/structure", structure;
      "lookup", lookup;
      "narrowing", narrowing;
      "bytes", nul;
    ]
