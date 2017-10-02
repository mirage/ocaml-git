type 'a io = 'a Lwt.t
type raw = Cstruct.t
type uri = Uri.t

type req =
  { req : Cohttp.Request.t
  ; query : (string * string list) list
  ; body : (raw * int * int) option -> unit Lwt.t }

type resp =
  { resp : Cohttp.Response.t
  ; body : Cohttp_lwt.Body.t }

module HTTP :
sig
  type headers = Cohttp.Header.t
  type path = string list
  type meth = Cohttp.Code.meth
  type version = int * int

  module Headers :
  sig
    type name = string

    val name       : string -> string
    val name_equal : String.t -> String.t -> bool

    val empty      : Cohttp.Header.t
    val is_empty   : 'a -> 'b

    val find       : string -> Cohttp.Header.t -> string option

    val def        : string -> string -> Cohttp.Header.t -> Cohttp.Header.t
    val def_multi  : string -> string list -> Cohttp.Header.t -> Cohttp.Header.t

    val get        : string -> Cohttp.Header.t -> string

    val merge      : Cohttp.Header.t -> Cohttp.Header.t -> Cohttp.Header.t

    val user_agent                   : string
    val content_type                 : string
    val access_control_allow_origin  : string
    val access_control_allow_methods : string
    val access_control_allow_headers : string
  end
end

type status = int

val s100_continue : int
val s200_ok       : int

module Request :
sig
  type body = (raw * int * int) option -> unit Lwt.t

  val with_headers : req -> Cohttp.Header.t -> req
  val with_path    : req -> string list -> req
  val with_body    : req -> ((raw * int * int) option -> unit Lwt.t) -> req

  val v :
    ?version:int * int ->
    HTTP.meth ->
    path:string list ->
    ?query:(string * string list) list ->
    HTTP.headers ->
    ((raw * int * int) option -> unit Lwt.t) -> req

  val headers : req -> Cohttp.Header.t
  val body    : req -> (raw * int * int) option -> unit Lwt.t
  val uri     : req -> Uri.t
  val meth    : req -> Cohttp.Code.meth
end

module Response :
sig
  type body = unit -> (raw * int * int) option Lwt.t

  val body   : resp -> unit -> (Cstruct.t * int * int) option Lwt.t
  val status : resp -> int
end
