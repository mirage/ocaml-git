(* FIXME: add copyrights *)

module type S = sig
  type +'a io

  (** The type for HTTP requests. *)
  type req

  (** The type for HTTP responses. *)
  type resp

  (** The type of the buffer. *)
  type raw

  type uri

  module HTTP : sig
    (** The type for header maps. A header map represents a list of HTTP
        headers. It maps header field names to their value. *)
    type headers

    (** The type for HTTP [absolute_path]s represented by its non-empty list of
        URI [segment]s. Note that URI segments can be empty; in particular the
        absolute path ["/"] is the list with a single empty segment and is thus
        represented by the list [[""]]. *)
    type path = string list

    (** The type for representing [HTTP-Version] fields. Both integers must be
        in the interval [\[0;9\]]. *)
    type version = int * int

    (** The type for HTTP request methods. *)
    type meth =
      [ `GET
      | `HEAD
      | `POST
      | `PUT
      | `DELETE
      | `CONNECT
      | `OPTIONS
      | `TRACE
      | `PATCH
      | `Other of string ]

    (** Headers. FIXME(dbuenzli): The semantic of this module considers than a
        field can contain multiple values. However, it's should be the wrong
        way to abstract values of headers - and let the user to consider than
        specific field can have multiple values. *)
    module Headers : sig
      (** The type for lower-cased HTTP header [field-name]s. *)
      type name

      val name : string -> name
      (** [name s] is a header name from [s]. @raise Invalid_argument if [s] is
          not a field-name. *)

      val name_equal : name -> name -> bool
      (** [name_equal n n'] is [true] iff [n] and [n'] are equal. *)

      (** {1:map Header maps} *)

      val empty : headers
      (** [empty] is the empty header map. *)

      val is_empty : headers -> bool
      (** [is_empty hs] is [true] iff [hs] is the empty header map. *)

      val find : name -> headers -> string option
      (** [find n hs] is the value of header [n] in [hs] (if defined). If [n]
          is multi-valued and defined, we return [String.concat "," vs] where
          [vs] are values related to [n] in [hs]. *)

      val get : name -> headers -> string
      (** [get n hs] is like {!find} but @raise Invalid_argument if [n] is
          undefined in [hs]. *)

      val pp : headers Fmt.t
      (** Pretty-printer of headers. *)

      (** {1:hcst Header name values} *)

      val user_agent : name
      (** User-Agent. *)

      val content_type : name
      (** Content-Type. *)

      val access_control_allow_origin : name
      (** Access-Control-Allow-Origin. *)

      val access_control_allow_methods : name
      (** Access-Control-Allow-Methods. *)

      val access_control_allow_headers : name
      (** Access-Control-Allow-Headers. *)

      val def : name -> string -> headers -> headers
      (** [def n v hs] is [hs] with [n] bound to [v]. *)

      val def_multi : name -> string list -> headers -> headers
      (** [def_multi n vs hs] is [hs] with [n] bound to the multi-value [vs].
          @raise Invalid_argument if [vs] is [[]]. *)

      val merge : headers -> headers -> headers
    end
  end

  (** {1:status_codes Status codes} *)

  (** The type for HTTP status code. *)
  type status = int

  val s100_continue : status
  (** [100]. *)

  val s200_ok : status
  (** [200]. *)

  module Request : sig
    (** The type for request bodies. *)
    type body

    val body : req -> body
    val meth : req -> HTTP.meth
    val uri : req -> uri

    val headers : req -> HTTP.headers
    (** [headers r] is [r]'s information. Initially empty it can be used be
        services and layers to store and share data. *)

    val with_headers : req -> HTTP.headers -> req
    (** [with_headers req hs] is [req] with headers [hs]. *)

    val with_path : req -> HTTP.path -> req
    (** [with_path req p] is [req] with path [p]. *)

    val with_body : req -> body -> req
    (** [with_body req body] is [req] with body [body]. *)

    val v :
         ?version:HTTP.version
      -> HTTP.meth
      -> path:HTTP.path
      -> ?query:(string * string list) list
      -> HTTP.headers
      -> body
      -> req
    (** [v meth ~path headers body] is an HTTP request with the given
        components. [query] defaults to [None]. *)
  end

  module Response : sig
    (** The type for request bodies.contents A body is a function that yields
        byte chunks of the request body as [Some (bytes, pos, len)] values. The
        bytes value must not be modified and is readable from [pos] to
        [pos+len] until the next call to the function. The function returns
        [None] at the end of stream. *)
    type body

    (* = unit -> (raw * int * int) option io *)

    val body : resp -> body
    (** [body r] is [r]'s body. *)

    val status : resp -> status
    (** [status r] is [r]'s headers. *)
  end
end
