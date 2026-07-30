let ( >>= ) = Result.bind

(* Character classes. These are the ASCII ones git's own parser uses; bytes
   above 127 are never alphanumeric here, whatever the locale says.

   Whitespace is exactly space, tab, newline and carriage return. C's isspace
   also counts a vertical tab and a form feed, but git replaces the C library's
   table with its own, in which it does not, so those two bytes are ordinary
   data: they stay in a value instead of being trimmed, and they are a parse
   error anywhere only whitespace may appear. *)

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

(* C's isspace, which is a WIDER class than the one above. It is not used by the
   file parser at all, only where git hands a value to strtoimax, which is libc
   and so does count a vertical tab and a form feed. The two tables meeting in
   one pipeline is why "\011" ^ "5" keeps its byte as a value and still reads as
   the integer 5. *)
let is_c_space = function
  | ' ' | '\t' | '\n' | '\011' | '\012' | '\r' -> true
  | _ -> false

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let is_digit = function '0' .. '9' -> true | _ -> false
let is_alnum c = is_alpha c || is_digit c
let is_keychar c = is_alnum c || c = '-'
let is_control c = Char.code c < 0x20 || Char.code c = 0x7f

(* String.exists and String.fold_left are more recent than the 4.08 we support. *)
let str_exists p s =
  Seq.fold_left (fun acc c -> acc || p c) false (String.to_seq s)

let str_last s = Seq.fold_left (fun _ c -> Some c) None (String.to_seq s)

let str_first s =
  match String.to_seq s () with Seq.Nil -> None | Seq.Cons (c, _) -> Some c

let string_of_chars l = String.of_seq (List.to_seq l)
let string_of_rev l = string_of_chars (List.rev l)

type value = Bare | Value of string

(* {2 Reading} *)

(* The input is a cursor over the bytes, plus the line number git would report.
   [line] counts from 1 and follows git: it is bumped for every newline read,
   including the virtual one the end of the input stands in for. *)
type input = { chars : char Seq.t; line : int }
type step = { c : char; eof : bool; rest : input }

(* git's get_next_char. A CRLF pair reads as a single '\n'; a CR on its own
   reads as itself; the end of the input reads as '\n' with [eof] set, which is
   how a last line without a newline still terminates a value. *)
let next i =
  match i.chars () with
  | Seq.Nil -> { c = '\n'; eof = true; rest = { i with line = i.line + 1 } }
  | Seq.Cons ('\n', tl) ->
      { c = '\n'; eof = false; rest = { chars = tl; line = i.line + 1 } }
  | Seq.Cons ('\r', tl) -> (
      match tl () with
      | Seq.Cons ('\n', tl') ->
          { c = '\n'; eof = false; rest = { chars = tl'; line = i.line + 1 } }
      | Seq.Nil | Seq.Cons (_, _) ->
          { c = '\r'; eof = false; rest = { i with chars = tl } })
  | Seq.Cons (c, tl) -> { c; eof = false; rest = { i with chars = tl } }

let bad_line line = Error (`Msg (Fmt.str "bad config line %d" line))

module Key = struct
  type t = { section : string; subsection : string option; name : string }

  let section k = k.section
  let subsection k = k.subsection
  let name k = k.name

  let to_string k =
    match k.section, k.subsection with
    | "", None -> k.name
    | s, None -> s ^ "." ^ k.name
    | s, Some sub -> s ^ "." ^ sub ^ "." ^ k.name

  let pp ppf k = Fmt.string ppf (to_string k)

  let compare a b =
    match String.compare a.section b.section with
    | 0 -> (
        match Option.compare String.compare a.subsection b.subsection with
        | 0 -> String.compare a.name b.name
        | n -> n)
    | n -> n

  let equal a b = compare a b = 0
  let no_keychar s = str_exists (fun c -> not (is_keychar c)) s
  let valid_section s = s <> "" && not (no_keychar s)

  let valid_name n =
    Option.fold ~none:false
      ~some:(fun c -> is_alpha c && not (no_keychar n))
      (str_first n)

  let has_newline =
    Option.fold ~none:false ~some:(str_exists (fun c -> c = '\n'))

  let v ?subsection section name =
    if not (valid_section section) then
      Error (`Msg (Fmt.str "invalid section %S" section))
    else if not (valid_name name) then
      Error (`Msg (Fmt.str "invalid variable name %S" name))
    else if has_newline subsection then
      Error (`Msg "a subsection cannot contain a newline")
    else
      Ok
        {
          section = String.lowercase_ascii section;
          subsection;
          name = String.lowercase_ascii name;
        }

  (* git splits a key at its LAST dot, so the subsection is everything in
     between and may contain dots of its own. *)
  let rec split_last acc = function
    | [] -> None
    | [ x ] -> Some (List.rev acc, x)
    | x :: tl -> split_last (x :: acc) tl

  let of_string s =
    let no_section =
      Error (`Msg (Fmt.str "key does not contain a section: %s" s))
    in
    let invalid = Error (`Msg (Fmt.str "invalid key: %s" s)) in
    let build section mid name =
      let subsection =
        match mid with [] -> None | _ :: _ -> Some (String.concat "." mid)
      in
      if section = "" && subsection = None then no_section
      else if name = "" then
        Error (`Msg (Fmt.str "key does not contain variable name: %s" s))
      else if not (valid_name name) then invalid
      else if section <> "" && not (valid_section section) then invalid
      else if has_newline subsection then invalid
      else
        Ok
          {
            section = String.lowercase_ascii section;
            subsection;
            name = String.lowercase_ascii name;
          }
    in
    match String.split_on_char '.' s with
    | [] | [ _ ] -> no_section
    | section :: (_ :: _ as rest) ->
        Option.fold ~none:no_section
          ~some:(fun (mid, name) -> build section mid name)
          (split_last [] rest)
end

type t = (Key.t * value) list

let entries t = t

(* The subsection of an extended header. Escapes here are NOT the ones a value
   uses: a backslash quotes any character at all, and only a newline is an
   error. *)
let rec ext_subsection acc input =
  let s = next input in
  if s.c = '\n' then bad_line (s.rest.line - 1)
  else if s.c = '"' then Ok (string_of_rev acc, s.rest)
  else if s.c = '\\' then
    let e = next s.rest in
    if e.c = '\n' then bad_line (e.rest.line - 1)
    else ext_subsection (e.c :: acc) e.rest
  else ext_subsection (s.c :: acc) s.rest

(* Whitespace inside a header means an extended header follows, and git then
   insists on the exact shape [section "subsection"]. *)
let rec ext_skip_space c input =
  if c = '\n' then bad_line (input.line - 1)
  else
    let s = next input in
    if is_space s.c then ext_skip_space s.c s.rest else Ok (s.c, s.rest)

let extended_base_var c input =
  ext_skip_space c input >>= fun (c, input) ->
  if c <> '"' then bad_line input.line
  else
    ext_subsection [] input >>= fun (sub, input) ->
    let s = next input in
    if s.c = ']' then Ok (Some sub, s.rest) else bad_line s.rest.line

(* git's get_base_var: the section name, lower-cased, up to ']' or the
   whitespace that introduces a subsection. *)
let rec base_var acc input =
  let s = next input in
  if s.eof then bad_line s.rest.line
  else if s.c = ']' then Ok ((string_of_rev acc, None), s.rest)
  else if is_space s.c then
    extended_base_var s.c s.rest >>= fun (sub, rest) ->
    Ok ((string_of_rev acc, sub), rest)
  else if is_keychar s.c || s.c = '.' then
    base_var (Char.lowercase_ascii s.c :: acc) s.rest
  else bad_line s.rest.line

(* A header contributes a dotted prefix; a key is then matched by splitting
   that prefix at its FIRST dot, which is what makes [a.b] and [a "b"] name the
   same variable. *)
let split_prefix p =
  match String.split_on_char '.' p with
  | [] -> "", None
  | [ s ] -> s, None
  | s :: rest -> s, Some (String.concat "." rest)

(* git's parse_value. [committed] is the value so far, reversed. [pending]
   holds whitespace that is only kept if something else follows it, which is
   how a trailing run gets trimmed while an interior one survives byte for
   byte. Anything that commits — including a quote or an escape — clears the
   pending run, so an ESCAPED trailing tab is kept and a value ending in a
   quote keeps the spaces before it. *)
let rec parse_value ~committed ~pending ~quote ~comment input =
  let s = next input in
  if s.c = '\n' then
    if quote then bad_line (s.rest.line - 1)
    else Ok (string_of_rev committed, s.rest)
  else if comment then parse_value ~committed ~pending ~quote ~comment s.rest
  else if is_space s.c && not quote then
    match committed with
    | [] -> parse_value ~committed ~pending ~quote ~comment s.rest
    | _ :: _ ->
        parse_value ~committed ~pending:(s.c :: pending) ~quote ~comment s.rest
  else if (not quote) && (s.c = ';' || s.c = '#') then
    parse_value ~committed ~pending ~quote ~comment:true s.rest
  else
    (* rev_append, not (@): the pending run is as long as the file cares to make
       it, and List.append is not tail-recursive below OCaml 5.1. *)
    let committed = List.rev_append (List.rev pending) committed in
    if s.c = '\\' then
      let e = next s.rest in
      let add ch =
        parse_value ~committed:(ch :: committed) ~pending:[] ~quote ~comment
          e.rest
      in
      match e.c with
      | '\n' -> parse_value ~committed ~pending:[] ~quote ~comment e.rest
      | 't' -> add '\t'
      | 'b' -> add '\b'
      | 'n' -> add '\n'
      | '\\' -> add '\\'
      | '"' -> add '"'
      | _ -> bad_line e.rest.line
    else if s.c = '"' then
      parse_value ~committed ~pending:[] ~quote:(not quote) ~comment s.rest
    else
      parse_value ~committed:(s.c :: committed) ~pending:[] ~quote ~comment
        s.rest

let rec key_chars acc input =
  let s = next input in
  if s.eof then string_of_rev acc, s.c, s.rest
  else if is_keychar s.c then key_chars (Char.lowercase_ascii s.c :: acc) s.rest
  else string_of_rev acc, s.c, s.rest

(* Only a space or a tab may sit between the name and '='. A CR is an error here
   even though it counts as whitespace everywhere else in the file. *)
let rec skip_blanks c input =
  if c = ' ' || c = '\t' then
    let s = next input in
    skip_blanks s.c s.rest
  else c, input

let variable first input =
  let name, c, rest = key_chars [ Char.lowercase_ascii first ] input in
  let c, rest = skip_blanks c rest in
  if c = '\n' then Ok ((name, Bare), rest)
  else if c <> '=' then bad_line rest.line
  else
    parse_value ~committed:[] ~pending:[] ~quote:false ~comment:false rest
    >>= fun (v, rest) -> Ok ((name, Value v), rest)

(* A UTF-8 BOM is skipped, but only a whole one and only at the very start. *)
type bom = Bom_start | Bom_partial of char list | Bom_done

let rec top ~bom ~comment ~prefix ~acc input =
  let s = next input in
  match bom with
  | Bom_start when s.c = '\xef' ->
      top ~bom:(Bom_partial [ '\xbb'; '\xbf' ]) ~comment ~prefix ~acc s.rest
  | Bom_partial (b :: bs) ->
      if s.c <> b then bad_line s.rest.line
      else
        let bom = match bs with [] -> Bom_done | _ :: _ -> Bom_partial bs in
        top ~bom ~comment ~prefix ~acc s.rest
  | Bom_start | Bom_partial [] | Bom_done -> line ~comment ~prefix ~acc s

and line ~comment ~prefix ~acc s =
  let continue_ ?(comment = comment) ?(prefix = prefix) ?(acc = acc) rest =
    top ~bom:Bom_done ~comment ~prefix ~acc rest
  in
  if s.c = '\n' then
    if s.eof then Ok (List.rev acc) else continue_ ~comment:false s.rest
  else if comment then continue_ s.rest
  else if is_space s.c then continue_ s.rest
  else if s.c = '#' || s.c = ';' then continue_ ~comment:true s.rest
  else if s.c = '[' then
    base_var [] s.rest >>= fun ((base, sub), rest) ->
    let full = base ^ Option.fold ~none:"" ~some:(fun x -> "." ^ x) sub in
    (* [] is rejected, but [ "x"] is not: it names an empty section. *)
    if String.length full < 1 then bad_line rest.line
    else continue_ ~prefix:(split_prefix full) rest
  else if not (is_alpha s.c) then bad_line s.rest.line
  else
    variable s.c s.rest >>= fun ((name, v), rest) ->
    let section, subsection = prefix in
    continue_ ~acc:(({ Key.section; subsection; name }, v) :: acc) rest

let of_string s =
  top ~bom:Bom_start ~comment:false ~prefix:("", None) ~acc:[]
    { chars = String.to_seq s; line = 1 }

(* {2 Looking up} *)

let get_all t k =
  List.filter_map (fun (k', v) -> if Key.equal k k' then Some v else None) t

(* git reports the LAST setting of a variable, not the first. *)
let get t k = List.fold_left (fun _ v -> Some v) None (get_all t k)
let mem t k = List.exists (fun (k', _) -> Key.equal k k') t

(* {2 Writing} *)

let escape f s = String.concat "" (List.of_seq (Seq.map f (String.to_seq s)))

let escape_subsection =
  escape (function '"' -> "\\\"" | '\\' -> "\\\\" | c -> String.make 1 c)

let escape_value =
  escape (function
    | '"' -> "\\\""
    | '\\' -> "\\\\"
    | '\n' -> "\\n"
    | '\t' -> "\\t"
    | '\b' -> "\\b"
    | c -> String.make 1 c)

(* Quote whenever leaving the value bare would change it on the way back in:
   an empty value, whitespace at either end (it would be trimmed), a comment
   character (it would start a comment), a quote or backslash (they are
   syntax), or any control byte. Inside quotes every byte survives, including a
   CR, because the closing quote keeps it away from the newline. *)
let needs_quote v =
  v = ""
  || Option.fold ~none:false ~some:is_space (str_first v)
  || Option.fold ~none:false ~some:is_space (str_last v)
  || str_exists
       (fun c -> c = '"' || c = '\\' || c = '#' || c = ';' || is_control c)
       v

let encode_value v = if needs_quote v then "\"" ^ escape_value v ^ "\"" else v

let pp_value ppf = function
  | Bare -> ()
  | Value v -> Fmt.string ppf (encode_value v)

let to_string t =
  let header (section, subsection) =
    Option.fold ~none:(Fmt.str "[%s]\n" section)
      ~some:(fun s -> Fmt.str "[%s \"%s\"]\n" section (escape_subsection s))
      subsection
  in
  let entry k v =
    match v with
    | Bare -> Fmt.str "\t%s\n" (Key.name k)
    | Value s -> Fmt.str "\t%s = %s\n" (Key.name k) (encode_value s)
  in
  let rendered, _ =
    List.fold_left
      (fun (acc, current) (k, v) ->
        let this = Key.section k, Key.subsection k in
        let acc =
          (* A variable with no section can only be one that came before the
             first header, so it needs no header of its own. *)
          if Some this = current || this = ("", None) then acc
          else header this :: acc
        in
        entry k v :: acc, Some this)
      ([], None) t
  in
  String.concat "" (List.rev rendered)

let pp ppf t = Fmt.string ppf (to_string t)

(* {2 Interpreting} *)

(* git reads numbers with strtoimax(…, 0), so the base comes from the prefix,
   and then takes at most a one-letter binary unit. *)

let digit_char_value c =
  if is_digit c then Some (Char.code c - Char.code '0')
  else if c >= 'a' && c <= 'f' then Some (Char.code c - Char.code 'a' + 10)
  else if c >= 'A' && c <= 'F' then Some (Char.code c - Char.code 'A' + 10)
  else None

let is_base_digit base c =
  Option.fold ~none:false ~some:(fun v -> v < base) (digit_char_value c)

let rec take_digits base acc l =
  match l with
  | [] -> List.rev acc, []
  | c :: tl ->
      if is_base_digit base c then take_digits base (c :: acc) tl
      else List.rev acc, l

let rec drop_while p l =
  match l with c :: tl when p c -> drop_while p tl | [] | _ :: _ -> l

let rec strip_zeros l =
  match l with
  | [] -> []
  | '0' :: (_ :: _ as tl) -> strip_zeros tl
  | c :: tl -> c :: tl

let unit_shift = function
  | "" -> Some 0
  | "k" | "K" -> Some 10
  | "m" | "M" -> Some 20
  | "g" | "G" -> Some 30
  | _ -> None

(* "0x" needs a hex digit after it to count as a prefix; without one strtoimax
   stops after the '0' and leaves the 'x' behind as a bogus unit, which is why
   "0x" and "08" are both errors. *)
let base_and_digits l =
  match l with
  | '0' :: ('x' | 'X') :: (d :: _ as rest) when is_base_digit 16 d ->
      let digits, tail = take_digits 16 [] rest in
      16, digits, tail
  | '0' :: rest ->
      let digits, tail = take_digits 8 [] rest in
      8, '0' :: digits, tail
  | [] | _ :: _ ->
      let digits, tail = take_digits 10 [] l in
      10, digits, tail

(* Fold the digits, having first rejected any that cannot fit, so that the
   accumulation itself cannot overflow. The comparison is on the written form,
   which keeps this free of division. *)
let magnitude base digits =
  let written = String.lowercase_ascii (string_of_chars (strip_zeros digits)) in
  let largest =
    match base with
    | 8 -> Printf.sprintf "%Lo" Int64.max_int
    | 16 -> Printf.sprintf "%Lx" Int64.max_int
    | _ -> Int64.to_string Int64.max_int
  in
  let fits =
    String.length written < String.length largest
    || String.length written = String.length largest
       && String.compare written largest <= 0
  in
  if not fits then None
  else
    Some
      (List.fold_left
         (fun acc c ->
           Option.fold ~none:acc
             ~some:(fun d ->
               Int64.add (Int64.mul acc (Int64.of_int base)) (Int64.of_int d))
             (digit_char_value c))
         0L digits)

let parse_number ~bound s =
  let bad = Error (`Msg (Fmt.str "bad numeric config value %S" s)) in
  (* strtoimax's leading-whitespace skip, so C's table rather than git's. *)
  let l = drop_while is_c_space (List.of_seq (String.to_seq s)) in
  let negative, l =
    match l with
    | '-' :: tl -> true, tl
    | '+' :: tl -> false, tl
    | [] | _ :: _ -> false, l
  in
  let base, digits, tail = base_and_digits l in
  match digits with
  | [] -> bad
  | _ :: _ ->
      Option.fold ~none:bad
        ~some:(fun shift ->
          Option.fold ~none:bad
            ~some:(fun m ->
              (* A negative number may reach one past the positive bound. *)
              let limit =
                if negative && bound <> Int64.max_int then Int64.add bound 1L
                else bound
              in
              if Int64.compare m (Int64.shift_right_logical limit shift) > 0
              then bad
              else
                let v = Int64.shift_left m shift in
                Ok (if negative then Int64.neg v else v))
            (magnitude base digits))
        (unit_shift (string_of_chars tail))

let int = function
  | Bare -> Error (`Msg "bad numeric config value \"\"")
  | Value s ->
      parse_number ~bound:(Int64.of_int max_int) s >>= fun n ->
      Ok (Int64.to_int n)

(* The integer fallback of a boolean goes through git's C int, so it is capped
   at 32 bits even where [int] above is not. *)
let bool = function
  | Bare -> Ok true
  | Value s -> (
      match String.lowercase_ascii s with
      | "" -> Ok false
      | "true" | "yes" | "on" -> Ok true
      | "false" | "no" | "off" -> Ok false
      | _ ->
          let bad = Error (`Msg (Fmt.str "bad boolean config value %S" s)) in
          Result.fold
            ~ok:(fun n -> Ok (Int64.compare n 0L <> 0))
            ~error:(fun _ -> bad)
            (parse_number ~bound:2147483647L s))

let get_with f t k =
  Option.fold ~none:(Ok None)
    ~some:(fun v -> f v >>= fun x -> Ok (Some x))
    (get t k)

let get_bool t k = get_with bool t k
let get_int t k = get_with int t k
