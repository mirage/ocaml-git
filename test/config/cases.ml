(* The git configuration file format.

   Every expectation below was recorded from git itself (git 2.50.1) rather
   than written by hand: each input was run through

     git config --file <case> --list -z

   for the parsing cases, and through `git config --file <case> --type=<t>
   --get a.x` for the typed ones. Cases that git rejects are pinned with the
   line number git reports, since that is part of what a caller sees. *)

module C = Git.Config

let value_t =
  let pp ppf = function
    | C.Bare -> Fmt.string ppf "<no value>"
    | C.Value v -> Fmt.pf ppf "%S" v
  in
  Alcotest.testable pp ( = )

let entries_t = Alcotest.(list (pair string value_t))
let parse_t = Alcotest.(result entries_t string)
let rendered t = List.map (fun (k, v) -> C.Key.to_string k, v) (C.entries t)

let parse s =
  let r : (C.t, [ `Msg of string ]) result = C.of_string s in
  Result.fold ~ok:(fun t -> Ok (rendered t)) ~error:(fun (`Msg m) -> Error m) r

let case name input expected =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.check parse_t name expected (parse input))

(* Parsing [s] and rendering it back must describe the same variables. *)
let roundtrip name input =
  Alcotest.test_case name `Quick (fun () ->
      let r : (C.t, [ `Msg of string ]) result = C.of_string input in
      Result.fold
        ~error:(fun (`Msg m) -> Alcotest.failf "%s: %s" name m)
        ~ok:(fun t ->
          Alcotest.check parse_t name (Ok (rendered t)) (parse (C.to_string t)))
        r)

let bool_case name v expected =
  Alcotest.test_case name `Quick (fun () ->
      let got =
        Result.fold ~ok:(fun b -> Some b) ~error:(fun _ -> None) (C.bool v)
      in
      Alcotest.(check (option bool)) name expected got)

(* Expectations are int64 so that the cases needing more than 31 bits still
   compile on a 32-bit host.

   [fits] then narrows a recorded expectation git can represent but an OCaml
   [int] cannot into "expect an error", because that is exactly what [C.int]
   does and the boundary moves with the host word size. That narrowing is
   deliberate rather than an accident of the harness, so it is pinned on its own
   in test.ml ("int is narrower than git's") instead of only being implied
   here. *)
let int_case name v expected =
  Alcotest.test_case name `Quick (fun () ->
      let fits e =
        Int64.compare e (Int64.of_int max_int) <= 0
        && Int64.compare e (Int64.of_int min_int) >= 0
      in
      let expected =
        Option.fold ~none:None
          ~some:(fun e -> if fits e then Some e else None)
          expected
      in
      let got =
        Result.fold
          ~ok:(fun n -> Some (Int64.of_int n))
          ~error:(fun _ -> None)
          (C.int v)
      in
      Alcotest.(check (option int64)) name expected got)

let parsing =
  [
    (* the ordinary shape *)
    case "sect-basic" "[a]\n\tb = c\n" (Ok [ "a.b", C.Value "c" ]);
    (* a header and a variable may share a line *)
    case "sect-same-line" "[a] b = c\n" (Ok [ "a.b", C.Value "c" ]);
    (* deprecated dotted subsection *)
    case "sect-dotted" "[a.b]\n\tx = 1\n" (Ok [ "a.b.x", C.Value "1" ]);
    (* dotted form is lowercased wholesale *)
    case "sect-dotted-case" "[A.B]\n\tx = 1\n" (Ok [ "a.b.x", C.Value "1" ]);
    (* quoted subsection keeps its case *)
    case "sect-sub" "[a \"B\"]\n\tx = 1\n" (Ok [ "a.B.x", C.Value "1" ]);
    (* \' inside a subsection *)
    case "sect-sub-esc-quote" "[a \"b\\\"c\"]\n\tx = 1\n"
      (Ok [ "a.b\"c.x", C.Value "1" ]);
    (* \\ inside a subsection *)
    case "sect-sub-esc-bslash" "[a \"b\\\\c\"]\n\tx = 1\n"
      (Ok [ "a.b\\c.x", C.Value "1" ]);
    (* any other escape drops the backslash *)
    case "sect-sub-esc-other" "[a \"b\\qc\"]\n\tx = 1\n"
      (Ok [ "a.bqc.x", C.Value "1" ]);
    (* ] must follow the closing quote *)
    case "sect-sub-space-before-bracket" "[a \"b\" ]\n\tx = 1\n"
      (Error "bad config line 1");
    (* whitespace starts an extended header, which needs a quote *)
    case "sect-space-only" "[a ]\n\tx = 1\n" (Error "bad config line 1");
    (* same, from the other side *)
    case "sect-leading-space" "[ a]\n\tx = 1\n" (Error "bad config line 1");
    (* empty header *)
    case "sect-empty" "[]\n\tx = 1\n" (Error "bad config line 1");
    (* EOF inside a header *)
    case "sect-unterminated" "[a" (Error "bad config line 2");
    (* EOF after the subsection, before ] *)
    case "sect-sub-unterminated" "[a \"b\"" (Error "bad config line 2");
    (* a newline inside a subsection *)
    case "sect-sub-nl-in-quote" "[a \"b\nc\"]\n" (Error "bad config line 1");
    (* a backslash-newline inside a subsection *)
    case "sect-sub-esc-nl" "[a \"b\\\n c\"]\n" (Error "bad config line 1");
    (* - is a key character *)
    case "sect-dash" "[a-b]\n\tx = 1\n" (Ok [ "a-b.x", C.Value "1" ]);
    (* _ is NOT a key character *)
    case "sect-underscore" "[a_b]\n\tx = 1\n" (Error "bad config line 1");
    (* sections may start with a digit *)
    case "sect-digit" "[1a]\n\tx = 1\n" (Ok [ "1a.x", C.Value "1" ]);
    (* a section may be reopened *)
    case "sect-reopened" "[a]\n\tx = 1\n[b]\n\ty = 2\n[a]\n\tz = 3\n"
      (Ok [ "a.x", C.Value "1"; "b.y", C.Value "2"; "a.z", C.Value "3" ]);
    (* variable on the header line, after a subsection *)
    case "sect-sub-same-line" "[a \"b\"]x = 1\n" (Ok [ "a.b.x", C.Value "1" ]);
    (* dotted section plus quoted subsection *)
    case "sect-sub-dotted-mix" "[a.b \"C\"]\n\tx = 1\n"
      (Ok [ "a.b.C.x", C.Value "1" ]);
    (* a subsection may contain dots *)
    case "sect-sub-with-dot" "[a \"b.c\"]\n\tx = 1\n"
      (Ok [ "a.b.c.x", C.Value "1" ]);
    (* an empty subsection *)
    case "sect-sub-empty" "[a \"\"]\n\tx = 1\n" (Ok [ "a..x", C.Value "1" ]);
    (* a variable with no '=' has NO value *)
    case "var-bare" "[core]\n\tbare\n" (Ok [ "core.bare", C.Bare ]);
    (* '=' with nothing after it is the empty string *)
    case "var-empty" "[core]\n\tbare =\n" (Ok [ "core.bare", C.Value "" ]);
    (* variable names are lowercased *)
    case "var-case" "[a]\n\tX = 1\n" (Ok [ "a.x", C.Value "1" ]);
    (* every character of a name is lowercased, not just the first *)
    case "var-case-multi" "[a]\n\txYz = 1\n" (Ok [ "a.xyz", C.Value "1" ]);
    (* - is legal in a variable name *)
    case "var-dash" "[a]\n\tx-y = 1\n" (Ok [ "a.x-y", C.Value "1" ]);
    (* a variable must start with a letter *)
    case "var-leading-digit" "[a]\n\t1x = 1\n" (Error "bad config line 2");
    (* _ is not legal in a variable name *)
    case "var-underscore" "[a]\n\tx_y = 1\n" (Error "bad config line 2");
    (* spacing around '=' is free *)
    case "var-spacing" "[a]\n\tx=1\n\ty =2\n\tz  =  3\n"
      (Ok [ "a.x", C.Value "1"; "a.y", C.Value "2"; "a.z", C.Value "3" ]);
    (* a variable before any header parses *)
    case "var-no-section" "x = 1\n[a]\n\ty = 2\n"
      (Ok [ "x", C.Value "1"; "a.y", C.Value "2" ]);
    (* two valueless variables *)
    case "var-two-bare" "[a]\n\tx\n\ty\n" (Ok [ "a.x", C.Bare; "a.y", C.Bare ]);
    (* a valueless variable at EOF *)
    case "var-eof-no-newline" "[a]\n\tx" (Ok [ "a.x", C.Bare ]);
    (* a variable may repeat; both are kept *)
    case "var-repeated" "[a]\n\tx = 1\n\tx = 2\n"
      (Ok [ "a.x", C.Value "1"; "a.x", C.Value "2" ]);
    (* a tab may precede '=' *)
    case "var-tab-before-eq" "[a]\n\tx\t= 1\n" (Ok [ "a.x", C.Value "1" ]);
    (* outer whitespace is trimmed *)
    case "val-trim" "[a]\n\tx =   spaced   \n" (Ok [ "a.x", C.Value "spaced" ]);
    (* interior whitespace is preserved VERBATIM *)
    case "val-interior-tab" "[a]\n\tx = one\ttwo   three\n"
      (Ok [ "a.x", C.Value "one\ttwo   three" ]);
    (* and in order: a mixed run is not reversed or normalised *)
    case "val-interior-mixed-ws" "[a]\n\tx = a \t  b\n"
      (Ok [ "a.x", C.Value "a \t  b" ]);
    (* quotes protect outer whitespace *)
    case "val-quoted-spaces" "[a]\n\tx = \"  a  \"\n"
      (Ok [ "a.x", C.Value "  a  " ]);
    (* quotes toggle, they do not delimit *)
    case "val-quote-toggle" "[a]\n\tx = a\"b\"c\n" (Ok [ "a.x", C.Value "abc" ]);
    (* \n is a newline *)
    case "val-esc-n" "[a]\n\tx = a\\nb\n" (Ok [ "a.x", C.Value "a\nb" ]);
    (* \t is a tab *)
    case "val-esc-t" "[a]\n\tx = a\\tb\n" (Ok [ "a.x", C.Value "a\tb" ]);
    (* \b is a backspace *)
    case "val-esc-b" "[a]\n\tx = a\\bb\n" (Ok [ "a.x", C.Value "a\bb" ]);
    (* \\ is a backslash *)
    case "val-esc-bslash" "[a]\n\tx = a\\\\b\n" (Ok [ "a.x", C.Value "a\\b" ]);
    (* \' is a quote *)
    case "val-esc-quote" "[a]\n\tx = a\\\"b\n" (Ok [ "a.x", C.Value "a\"b" ]);
    (* any other escape is an ERROR *)
    case "val-esc-bad" "[a]\n\tx = a\\qb\n" (Error "bad config line 2");
    (* backslash-newline continues the value *)
    case "val-continuation" "[a]\n\tx = one\\\ntwo\n"
      (Ok [ "a.x", C.Value "onetwo" ]);
    (* continuation works inside quotes too *)
    case "val-continuation-quoted" "[a]\n\tx = \"one\\\ntwo\"\n"
      (Ok [ "a.x", C.Value "onetwo" ]);
    (* # starts a comment *)
    case "val-comment-hash" "[a]\n\tx = a # b\n" (Ok [ "a.x", C.Value "a" ]);
    (* ; starts a comment *)
    case "val-comment-semi" "[a]\n\tx = a ; b\n" (Ok [ "a.x", C.Value "a" ]);
    (* # inside quotes is literal *)
    case "val-hash-quoted" "[a]\n\tx = \"a#b\"\n" (Ok [ "a.x", C.Value "a#b" ]);
    (* ; inside quotes is literal *)
    case "val-semi-quoted" "[a]\n\tx = \"a;b\"\n" (Ok [ "a.x", C.Value "a;b" ]);
    (* a newline inside quotes is an ERROR *)
    case "val-unterminated-quote" "[a]\n\tx = \"abc\n"
      (Error "bad config line 2");
    (* EOF inside quotes is an ERROR *)
    case "val-unterminated-quote-eof" "[a]\n\tx = \"abc"
      (Error "bad config line 2");
    (* a whitespace-only value is empty *)
    case "val-only-space" "[a]\n\tx =    \n" (Ok [ "a.x", C.Value "" ]);
    (* a comment may start immediately *)
    case "val-comment-immediate" "[a]\n\tx =# c\n" (Ok [ "a.x", C.Value "" ]);
    (* whitespace before a comment is trimmed *)
    case "val-trim-before-comment" "[a]\n\tx = a   # c\n"
      (Ok [ "a.x", C.Value "a" ]);
    (* a value that is only a comment *)
    case "val-semi-first" "[a]\n\tx = ;c\n" (Ok [ "a.x", C.Value "" ]);
    (* explicit empty string *)
    case "val-empty-quotes" "[a]\n\tx = \"\"\n" (Ok [ "a.x", C.Value "" ]);
    (* text after a closing quote *)
    case "val-quote-then-space" "[a]\n\tx = \"a\" b\n"
      (Ok [ "a.x", C.Value "a b" ]);
    (* an ESCAPED trailing tab is NOT trimmed *)
    case "val-esc-trailing-tab" "[a]\n\tx = a\\t\n"
      (Ok [ "a.x", C.Value "a\t" ]);
    (* escaped tab kept, real whitespace trimmed *)
    case "val-esc-tab-then-comment" "[a]\n\tx = a\\t # c\n"
      (Ok [ "a.x", C.Value "a\t" ]);
    (* a closing quote cancels the pending trim *)
    case "val-quote-cancels-trim" "[a]\n\tx = a  \"\"\n"
      (Ok [ "a.x", C.Value "a  " ]);
    (* a trailing backslash at EOF ends the value *)
    case "val-backslash-at-eof" "[a]\n\tx = a\\" (Ok [ "a.x", C.Value "a" ]);
    (* quotes protect a trailing space *)
    case "val-quoted-trailing-space" "[a]\n\tx = \"a \"\n"
      (Ok [ "a.x", C.Value "a " ]);
    (* a CR before the closing quote survives *)
    case "val-cr-before-close-quote" "[a]\n\tx = \"a\r\"\n"
      (Ok [ "a.x", C.Value "a\r" ]);
    (* whitespace then a quote = an empty section name *)
    case "sect-empty-base-sub" "[ \"a\"]\n\tx = 1\n"
      (Ok [ ".a.x", C.Value "1" ]);
    (* a UTF-8 BOM is skipped *)
    case "file-bom" "\xef\xbb\xbf[a]\n\tx = 1\n" (Ok [ "a.x", C.Value "1" ]);
    (* a partial BOM is an ERROR *)
    case "file-bom-partial" "\xef\xbb[a]\n\tx = 1\n" (Error "bad config line 1");
    (* a BOM elsewhere is not special *)
    case "file-bom-late" "[a]\n\xef\xbb\xbf\tx = 1\n"
      (Error "bad config line 2");
    (* CRLF line endings *)
    case "file-crlf" "[a]\r\n\tx = 1\r\n" (Ok [ "a.x", C.Value "1" ]);
    (* CRLF must end a comment, not just LF *)
    case "file-crlf-comment" "# c\r\n[a]\r\n\tx = 1\r\n"
      (Ok [ "a.x", C.Value "1" ]);
    (* CRLF must count lines *)
    case "file-crlf-error-line" "[a]\r\n\tx = 1\r\n\t1bad = 2\r\n"
      (Error "bad config line 3");
    (* a lone CR inside a value *)
    case "file-cr-in-value" "[a]\n\tx = a\rb\n" (Ok [ "a.x", C.Value "a\rb" ]);
    (* a lone CR where '=' was expected *)
    case "file-cr-before-eq" "[a]\n\tx\r= 1\n" (Error "bad config line 2");
    (* no trailing newline *)
    case "file-no-trailing-nl" "[a]\n\tx = 1" (Ok [ "a.x", C.Value "1" ]);
    (* an empty file *)
    case "file-empty" "" (Ok []);
    (* comments only *)
    case "file-comments-only" "# hi\n; there\n" (Ok []);
    (* blank lines are ignored *)
    case "file-blank-lines" "\n\n[a]\n\n\tx = 1\n\n" (Ok [ "a.x", C.Value "1" ]);
    (* file-nul-in-value is pinned in test.ml: git stops at the NUL, we do not *)
    (* UTF-8 in a value *)
    case "file-utf8-value" "[a]\n\tx = caf\xc3\xa9\n"
      (Ok [ "a.x", C.Value "caf\xc3\xa9" ]);
    (* UTF-8 in a subsection *)
    case "file-utf8-subsection" "[a \"caf\xc3\xa9\"]\n\tx = 1\n"
      (Ok [ "a.caf\xc3\xa9.x", C.Value "1" ]);
    (* invalid UTF-8 in a value *)
    case "file-high-byte-value" "[a]\n\tx = \xff\xfe\n"
      (Ok [ "a.x", C.Value "\xff\xfe" ]);
    (* a trailing VT is NOT trimmed: it is not whitespace *)
    case "file-vt-in-value" "[a]\n\tx = 5\x0b\n" (Ok [ "a.x", C.Value "5\x0b" ]);
    (* nor is a form feed *)
    case "file-ff-in-value" "[a]\n\tx = 5\x0c\n" (Ok [ "a.x", C.Value "5\x0c" ]);
    (* a VT where only whitespace may appear is an error *)
    case "file-vt-line-start" "[a]\n\x0b\tx = 1\n" (Error "bad config line 2");
    (* and between a name and its '=' *)
    case "file-vt-before-eq" "[a]\n\tx\x0b= 1\n" (Error "bad config line 2");
    (* and inside a header *)
    case "file-vt-in-header" "[a\x0b]\n\tx = 1\n" (Error "bad config line 1");
    (* backslash then a LONE CR is an unknown escape *)
    case "val-esc-lone-cr" "[a]\n\tx = a\\\rb\n" (Error "bad config line 2");
    (* but backslash-CRLF continues the line *)
    case "val-esc-crlf-continuation" "[a]\n\tx = a\\\r\nb\n"
      (Ok [ "a.x", C.Value "ab" ]);
    (* a trailing backslash at EOF inside quotes is an error *)
    case "val-backslash-eof-quoted" "[a]\n\tx = \"y\\"
      (Error "bad config line 3");
    (* a comment still fires on a continued line *)
    case "val-continuation-then-comment" "[a]\nb = x\\\n# y\nc = d\n"
      (Ok [ "a.b", C.Value "x"; "a.c", C.Value "d" ]);
    (* a header CANNOT be introduced by a continuation *)
    case "val-continuation-into-bracket" "[s]\nb = \\\n[a]\nc = d\n"
      (Ok [ "s.b", C.Value "[a]"; "s.c", C.Value "d" ]);
    (* a value-less variable may NOT carry a comment *)
    case "var-bare-then-comment" "[a]\n\tx # c\n" (Error "bad config line 2");
    (* likewise with a semicolon *)
    case "var-bare-then-semicolon" "[a]\n\tx ; c\n" (Error "bad config line 2");
    (* NUL is rejected inside a variable name *)
    case "file-nul-in-name" "[a]\n\tx\x00y = 1\n" (Error "bad config line 2");
    (* and inside a bare section name *)
    case "file-nul-in-section" "[a\x00b]\n\tx = 1\n" (Error "bad config line 1");
    (* several headers may share a line; the last one wins *)
    case "sect-two-headers-one-line" "[a] [b]\nc = d\n"
      (Ok [ "b.c", C.Value "d" ]);
    (* a trailing dot in a header gives an empty subsection *)
    case "sect-dot-trailing" "[a.]\n\tx = 1\n" (Ok [ "a..x", C.Value "1" ]);
    (* a CR in a header starts the extended form, which needs a quote *)
    case "sect-cr-in-header" "[a\r]\n\tx = 1\n" (Error "bad config line 1");
    (* backslash-CRLF inside a subsection is fatal *)
    case "sect-sub-esc-crlf" "[a \"b\\\r\nc\"]\n\tx = 1\n"
      (Error "bad config line 1");
    (* but backslash-CR puts a CR in the key *)
    case "sect-sub-esc-lone-cr" "[a \"b\\\rc\"]\n\tx = 1\n"
      (Ok [ "a.b\rc.x", C.Value "1" ]);
    (* a continuation cancels the pending trailing trim *)
    case "val-continuation-cancels-trim" "[a]\n\tx = y  \\\n\n"
      (Ok [ "a.x", C.Value "y  " ]);
    (* quoted spaces after unquoted ones keep all four *)
    case "val-quote-resets-trim-twice" "[a]\n\tx = a  \"  \"\n"
      (Ok [ "a.x", C.Value "a    " ]);
    (* a CR-only file is ONE line, so a comment swallows it *)
    case "file-cr-only-comment" "# c\r[a]\r\tx = 1\r" (Ok []);
    (* error on line 3 *)
    case "err-line3" "[a]\n\tx = 1\n\t1bad = 2\n" (Error "bad config line 3");
    (* error after a continued value *)
    case "err-line-after-continuation" "[a]\n\tx = one\\\ntwo\n\t1bad = 3\n"
      (Error "bad config line 4");
    (* unterminated quote line number *)
    case "err-unterminated-quote-line" "[a]\n\tx = 1\n\ty = \"abc\n\tz = 2\n"
      (Error "bad config line 3");
    (* unterminated header at EOF *)
    case "err-sect-line2" "[a]\n[b\n" (Error "bad config line 2");
  ]

let round_trips =
  [
    roundtrip "sect-basic" "[a]\n\tb = c\n";
    roundtrip "sect-same-line" "[a] b = c\n";
    roundtrip "sect-dotted" "[a.b]\n\tx = 1\n";
    roundtrip "sect-dotted-case" "[A.B]\n\tx = 1\n";
    roundtrip "sect-sub" "[a \"B\"]\n\tx = 1\n";
    roundtrip "sect-sub-esc-quote" "[a \"b\\\"c\"]\n\tx = 1\n";
    roundtrip "sect-sub-esc-bslash" "[a \"b\\\\c\"]\n\tx = 1\n";
    roundtrip "sect-sub-esc-other" "[a \"b\\qc\"]\n\tx = 1\n";
    roundtrip "sect-dash" "[a-b]\n\tx = 1\n";
    roundtrip "sect-digit" "[1a]\n\tx = 1\n";
    roundtrip "sect-reopened" "[a]\n\tx = 1\n[b]\n\ty = 2\n[a]\n\tz = 3\n";
    roundtrip "sect-sub-same-line" "[a \"b\"]x = 1\n";
    roundtrip "sect-sub-dotted-mix" "[a.b \"C\"]\n\tx = 1\n";
    roundtrip "sect-sub-with-dot" "[a \"b.c\"]\n\tx = 1\n";
    roundtrip "sect-sub-empty" "[a \"\"]\n\tx = 1\n";
    roundtrip "var-bare" "[core]\n\tbare\n";
    roundtrip "var-empty" "[core]\n\tbare =\n";
    roundtrip "var-case" "[a]\n\tX = 1\n";
    roundtrip "var-case-multi" "[a]\n\txYz = 1\n";
    roundtrip "var-dash" "[a]\n\tx-y = 1\n";
    roundtrip "var-spacing" "[a]\n\tx=1\n\ty =2\n\tz  =  3\n";
    roundtrip "var-no-section" "x = 1\n[a]\n\ty = 2\n";
    roundtrip "var-two-bare" "[a]\n\tx\n\ty\n";
    roundtrip "var-eof-no-newline" "[a]\n\tx";
    roundtrip "var-repeated" "[a]\n\tx = 1\n\tx = 2\n";
    roundtrip "var-tab-before-eq" "[a]\n\tx\t= 1\n";
    roundtrip "val-trim" "[a]\n\tx =   spaced   \n";
    roundtrip "val-interior-tab" "[a]\n\tx = one\ttwo   three\n";
    roundtrip "val-interior-mixed-ws" "[a]\n\tx = a \t  b\n";
    roundtrip "val-quoted-spaces" "[a]\n\tx = \"  a  \"\n";
    roundtrip "val-quote-toggle" "[a]\n\tx = a\"b\"c\n";
    roundtrip "val-esc-n" "[a]\n\tx = a\\nb\n";
    roundtrip "val-esc-t" "[a]\n\tx = a\\tb\n";
    roundtrip "val-esc-b" "[a]\n\tx = a\\bb\n";
    roundtrip "val-esc-bslash" "[a]\n\tx = a\\\\b\n";
    roundtrip "val-esc-quote" "[a]\n\tx = a\\\"b\n";
    roundtrip "val-continuation" "[a]\n\tx = one\\\ntwo\n";
    roundtrip "val-continuation-quoted" "[a]\n\tx = \"one\\\ntwo\"\n";
    roundtrip "val-comment-hash" "[a]\n\tx = a # b\n";
    roundtrip "val-comment-semi" "[a]\n\tx = a ; b\n";
    roundtrip "val-hash-quoted" "[a]\n\tx = \"a#b\"\n";
    roundtrip "val-semi-quoted" "[a]\n\tx = \"a;b\"\n";
    roundtrip "val-only-space" "[a]\n\tx =    \n";
    roundtrip "val-comment-immediate" "[a]\n\tx =# c\n";
    roundtrip "val-trim-before-comment" "[a]\n\tx = a   # c\n";
    roundtrip "val-semi-first" "[a]\n\tx = ;c\n";
    roundtrip "val-empty-quotes" "[a]\n\tx = \"\"\n";
    roundtrip "val-quote-then-space" "[a]\n\tx = \"a\" b\n";
    roundtrip "val-esc-trailing-tab" "[a]\n\tx = a\\t\n";
    roundtrip "val-esc-tab-then-comment" "[a]\n\tx = a\\t # c\n";
    roundtrip "val-quote-cancels-trim" "[a]\n\tx = a  \"\"\n";
    roundtrip "val-backslash-at-eof" "[a]\n\tx = a\\";
    roundtrip "val-quoted-trailing-space" "[a]\n\tx = \"a \"\n";
    roundtrip "val-cr-before-close-quote" "[a]\n\tx = \"a\r\"\n";
    roundtrip "sect-empty-base-sub" "[ \"a\"]\n\tx = 1\n";
    roundtrip "file-bom" "\xef\xbb\xbf[a]\n\tx = 1\n";
    roundtrip "file-crlf" "[a]\r\n\tx = 1\r\n";
    roundtrip "file-crlf-comment" "# c\r\n[a]\r\n\tx = 1\r\n";
    roundtrip "file-cr-in-value" "[a]\n\tx = a\rb\n";
    roundtrip "file-no-trailing-nl" "[a]\n\tx = 1";
    roundtrip "file-empty" "";
    roundtrip "file-comments-only" "# hi\n; there\n";
    roundtrip "file-blank-lines" "\n\n[a]\n\n\tx = 1\n\n";
    roundtrip "file-nul-in-value" "[a]\n\tx = a\x00b\n";
    roundtrip "file-utf8-value" "[a]\n\tx = caf\xc3\xa9\n";
    roundtrip "file-utf8-subsection" "[a \"caf\xc3\xa9\"]\n\tx = 1\n";
    roundtrip "file-high-byte-value" "[a]\n\tx = \xff\xfe\n";
    roundtrip "file-vt-in-value" "[a]\n\tx = 5\x0b\n";
    roundtrip "file-ff-in-value" "[a]\n\tx = 5\x0c\n";
    roundtrip "val-esc-crlf-continuation" "[a]\n\tx = a\\\r\nb\n";
    roundtrip "val-continuation-then-comment" "[a]\nb = x\\\n# y\nc = d\n";
    roundtrip "val-continuation-into-bracket" "[s]\nb = \\\n[a]\nc = d\n";
    roundtrip "sect-two-headers-one-line" "[a] [b]\nc = d\n";
    roundtrip "sect-dot-trailing" "[a.]\n\tx = 1\n";
    roundtrip "sect-sub-esc-lone-cr" "[a \"b\\\rc\"]\n\tx = 1\n";
    roundtrip "val-continuation-cancels-trim" "[a]\n\tx = y  \\\n\n";
    roundtrip "val-quote-resets-trim-twice" "[a]\n\tx = a  \"  \"\n";
    roundtrip "file-cr-only-comment" "# c\r[a]\r\tx = 1\r";
  ]

let bools =
  [
    bool_case "no value" C.Bare (Some true);
    bool_case "" (C.Value "") (Some false);
    bool_case "true" (C.Value "true") (Some true);
    bool_case "TRUE" (C.Value "TRUE") (Some true);
    bool_case "yes" (C.Value "yes") (Some true);
    bool_case "On" (C.Value "On") (Some true);
    bool_case "false" (C.Value "false") (Some false);
    bool_case "NO" (C.Value "NO") (Some false);
    bool_case "off" (C.Value "off") (Some false);
    bool_case "1" (C.Value "1") (Some true);
    bool_case "0" (C.Value "0") (Some false);
    bool_case "2" (C.Value "2") (Some true);
    bool_case "-1" (C.Value "-1") (Some true);
    bool_case "junk" (C.Value "junk") None;
    bool_case "0x10" (C.Value "0x10") (Some true);
    bool_case "010" (C.Value "010") (Some true);
    bool_case "08" (C.Value "08") None;
    bool_case "5k" (C.Value "5k") (Some true);
    bool_case "5K" (C.Value "5K") (Some true);
    bool_case "5m" (C.Value "5m") (Some true);
    bool_case "5g" (C.Value "5g") None;
    bool_case "5kb" (C.Value "5kb") None;
    bool_case "+5" (C.Value "+5") (Some true);
    bool_case "-5k" (C.Value "-5k") (Some true);
    bool_case "-0x10" (C.Value "-0x10") (Some true);
    bool_case "0x10k" (C.Value "0x10k") (Some true);
    bool_case "0x" (C.Value "0x") None;
    bool_case "5.5" (C.Value "5.5") None;
    bool_case "99999999999999999999" (C.Value "99999999999999999999") None;
    bool_case "2147483648" (C.Value "2147483648") None;
    bool_case " 5" (C.Value " 5") (Some true);
    bool_case "18446744073709551616" (C.Value "18446744073709551616") None;
    bool_case "18446744073709551617" (C.Value "18446744073709551617") None;
    bool_case "9223372036854775808" (C.Value "9223372036854775808") None;
    bool_case "9223372036854775807" (C.Value "9223372036854775807") None;
    bool_case "\\x0b5" (C.Value "\x0b5") (Some true);
    bool_case "\\x0c5" (C.Value "\x0c5") (Some true);
    bool_case "\\x0b5k" (C.Value "\x0b5k") (Some true);
    bool_case "5\\x0b" (C.Value "5\x0b") None;
  ]

let ints =
  [
    int_case "no value" C.Bare None;
    int_case "" (C.Value "") None;
    int_case "true" (C.Value "true") None;
    int_case "TRUE" (C.Value "TRUE") None;
    int_case "yes" (C.Value "yes") None;
    int_case "On" (C.Value "On") None;
    int_case "false" (C.Value "false") None;
    int_case "NO" (C.Value "NO") None;
    int_case "off" (C.Value "off") None;
    int_case "1" (C.Value "1") (Some 1L);
    int_case "0" (C.Value "0") (Some 0L);
    int_case "2" (C.Value "2") (Some 2L);
    int_case "-1" (C.Value "-1") (Some (-1L));
    int_case "junk" (C.Value "junk") None;
    int_case "0x10" (C.Value "0x10") (Some 16L);
    int_case "010" (C.Value "010") (Some 8L);
    int_case "08" (C.Value "08") None;
    int_case "5k" (C.Value "5k") (Some 5120L);
    int_case "5K" (C.Value "5K") (Some 5120L);
    int_case "5m" (C.Value "5m") (Some 5242880L);
    int_case "5g" (C.Value "5g") (Some 5368709120L);
    int_case "5kb" (C.Value "5kb") None;
    int_case "+5" (C.Value "+5") (Some 5L);
    int_case "-5k" (C.Value "-5k") (Some (-5120L));
    int_case "-0x10" (C.Value "-0x10") (Some (-16L));
    int_case "0x10k" (C.Value "0x10k") (Some 16384L);
    int_case "0x" (C.Value "0x") None;
    int_case "5.5" (C.Value "5.5") None;
    int_case "99999999999999999999" (C.Value "99999999999999999999") None;
    int_case "2147483648" (C.Value "2147483648") (Some 2147483648L);
    int_case " 5" (C.Value " 5") (Some 5L);
    int_case "18446744073709551616" (C.Value "18446744073709551616") None;
    int_case "18446744073709551617" (C.Value "18446744073709551617") None;
    int_case "9223372036854775808" (C.Value "9223372036854775808") None;
    int_case "9223372036854775807" (C.Value "9223372036854775807")
      (Some 9223372036854775807L);
    int_case "\\x0b5" (C.Value "\x0b5") (Some 5L);
    int_case "\\x0c5" (C.Value "\x0c5") (Some 5L);
    int_case "\\x0b5k" (C.Value "\x0b5k") (Some 5120L);
    int_case "5\\x0b" (C.Value "5\x0b") None;
  ]
