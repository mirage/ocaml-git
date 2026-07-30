(** The Git configuration file format.

    This module reads the format described in
    {{:https://git-scm.com/docs/git-config#_configuration_file} git-config(1)},
    the one used by [.git/config], [~/.gitconfig] and [.gitmodules]. It is a
    pure, byte-oriented parser: it neither looks at the file system nor merges
    the several configuration files git consults.

    The implementation follows git's own parser ([config.c]) rather than the
    prose documentation, because the two disagree in a few places. The
    surprising rules are documented on the functions they affect. *)

type value =
  | Bare
      (** A variable written without ['='], such as [bare] in:
          {v
[core]
	bare
          v}
          It carries no textual value; git reads it as the boolean [true]. *)
  | Value of string
      (** A variable written with ['='], carrying a (possibly empty) value.
          Values are byte strings: no character encoding is assumed. *)

val pp_value : value Fmt.t
(** [pp_value] formats a value the way it would appear in a file. *)

module Key : sig
  (** Fully-qualified variable names.

      A name has a [section], an optional [subsection] and a [name], and is
      written [section.name] or [section.subsection.name]. Sections and variable
      names are case-insensitive and are folded to lower case; a subsection is
      matched case-sensitively.

      Whether a subsection keeps its case depends on how the file spelled it.
      The quoted form keeps it, so a header naming the subsection ["Origin"] of
      section ["remote"] gives the variable [remote.Origin.url]. The deprecated
      dotted form is lower-cased along with the section name, so the same header
      written with a dot gives [remote.origin.url] instead, and neither variable
      can be reached under the other's spelling. *)

  type t
  (** The type for variable names. *)

  val v :
    ?subsection:string -> string -> string -> (t, [> `Msg of string ]) result
  (** [v ?subsection section name] is the key [section.subsection.name]. It is
      an error if [section] is empty or contains anything other than
      alphanumerics and ['-'], if [name] is empty, does not start with a letter
      or contains anything other than alphanumerics and ['-'], or if
      [subsection] contains a newline. *)

  val of_string : string -> (t, [> `Msg of string ]) result
  (** [of_string s] parses the dotted spelling accepted by [git config], such as
      ["core.bare"] or ["remote.origin.url"]. The {e last} dot separates the
      variable name, so everything between the first and the last dot is the
      subsection and may itself contain dots. *)

  val to_string : t -> string
  (** [to_string k] is the dotted spelling of [k]. *)

  val section : t -> string
  (** [section k] is the (lower-cased) section of [k]. *)

  val subsection : t -> string option
  (** [subsection k] is the subsection of [k], if any, with the case it was
      given. *)

  val name : t -> string
  (** [name k] is the (lower-cased) variable name of [k]. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] name the same variable. *)

  val compare : t -> t -> int
  (** [compare] is a total order on keys. *)

  val pp : t Fmt.t
  (** [pp] formats a key using its dotted spelling. *)
end

type t
(** The type for a parsed configuration file. *)

val of_string : string -> (t, [> `Msg of string ]) result
(** [of_string s] parses [s].

    The error carries git's own wording, ["bad config line %d"], with the same
    line number git reports.

    Parsing is byte-exact: values keep their bytes, including invalid UTF-8 and
    [NUL]. Note that git itself is not able to do this — it hands values to the
    rest of the program as C strings, so it silently truncates a value at the
    first [NUL]. This is the one place where we knowingly keep more than git
    does. *)

val entries : t -> (Key.t * value) list
(** [entries t] are the variables of [t] in file order, including repeated ones.
    A variable that appears before any section header is kept, with an empty
    section; git accepts such a file but cannot address those variables, and
    neither can {!get} since {!Key.v} refuses an empty section. *)

val get : t -> Key.t -> value option
(** [get t k] is the {e last} value bound to [k], which is the one
    [git config --get] reports when a variable is set several times. *)

val get_all : t -> Key.t -> value list
(** [get_all t k] is every value bound to [k], in file order. *)

val mem : t -> Key.t -> bool
(** [mem t k] is [true] if [k] is set at all, including without a value. *)

val to_string : t -> string
(** [to_string t] renders [t] back to the configuration file format.

    [of_string] and [to_string] round-trip: parsing the output yields the same
    entries. Comments, blank lines and the original spelling of values are
    {e not} preserved, so this is not a substitute for the surgical rewriting
    that [git config --replace-all] does. *)

val pp : t Fmt.t
(** [pp] formats a configuration file; it is {!to_string}. *)

val bool : value -> (bool, [> `Msg of string ]) result
(** [bool v] interprets [v] as a boolean, as [git config --type=bool] does.
    {!Bare} is [true] and the empty string is [false]; ["true"], ["yes"] and
    ["on"] are [true] and ["false"], ["no"] and ["off"] are [false], each
    case-insensitively. Anything else is interpreted as an {!int} and compared
    against zero.

    That last fallback is bounded differently from {!int}: git reads it with a C
    [int], so it stops at [2147483647] on any host, and ["5g"] and
    ["2147483648"] are errors {e here}. {!int} is bounded by the OCaml [int]
    instead, so on a 64-bit host it reads both of those and is the more
    permissive of the two, while on a 32-bit host, where an OCaml [int] stops at
    [1073741823], it rejects them as well and is the stricter one. *)

val int : value -> (int, [> `Msg of string ]) result
(** [int v] interprets [v] as an integer, as [git config --type=int] does.

    The syntax is C's [strtoimax] with a base of [0], so a leading ["0x"] means
    hexadecimal and a leading ["0"] means octal, and leading whitespace and a
    leading sign are accepted. A single trailing ['k'], ['m'] or ['g'], in
    either case, multiplies by 2{^ 10}, 2{^ 20} or 2{^ 30}; any other trailing
    text is an error, so ["5kb"] and ["5.5"] are rejected. The suffix combines
    with the base, so ["0x10k"] is [16384].

    The leading whitespace is C's notion of it, which is wider than the one the
    file parser uses: a value may begin with a vertical tab or a form feed,
    which the parser keeps as data, and this still skips it.

    The result must fit in an OCaml [int], which is narrower than git's own
    limit for this accessor on every platform: git reads it as an [int64], while
    an OCaml [int] is 63 bits on a 64-bit host and 31 on a 32-bit one. A value
    in between — [4611686018427387904] on a typical host, say — is an error here
    and a number to git. *)

val get_bool : t -> Key.t -> (bool option, [> `Msg of string ]) result
(** [get_bool t k] is [Ok None] if [k] is unset, and otherwise {!bool} applied
    to its last value. *)

val get_int : t -> Key.t -> (int option, [> `Msg of string ]) result
(** [get_int t k] is [Ok None] if [k] is unset, and otherwise {!int} applied to
    its last value. *)
