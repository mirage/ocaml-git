module type APPEND = sig
  type t

  type uid

  and fd

  type error

  type +'a fiber

  val pp_error : error Fmt.t

  val create : t -> uid -> (fd, error) result fiber

  val map : t -> fd -> pos:int64 -> int -> Bigstringaf.t fiber

  val append : t -> fd -> string -> unit fiber

  val move : t -> src:uid -> dst:uid -> (unit, error) result fiber

  val close : t -> fd -> (unit, error) result fiber
end

module type UID = sig
  include Carton.UID

  include Sigs.UID with type t := t

  val hash : t -> int
end

module type HTTP = sig
  type error

  val pp_error : error Fmt.t

  val get :
    resolvers:Conduit.resolvers ->
    ?headers:(string * string) list ->
    Uri.t ->
    (unit * string, error) result Lwt.t

  val post :
    resolvers:Conduit.resolvers ->
    ?headers:(string * string) list ->
    Uri.t ->
    string ->
    (unit * string, error) result Lwt.t
end

type endpoint = private {
  scheme :
    [ `SSH of string
    | `Git
    | `HTTP of (string * string) list
    | `HTTPS of (string * string) list ];
  path : string;
  domain_name : [ `host ] Domain_name.t;
}

val pp_endpoint : endpoint Fmt.t

val endpoint_of_string : string -> (endpoint, [> `Msg of string ]) result

module Make
    (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
    (Pack : APPEND with type +'a fiber = 'a Lwt.t)
    (Index : APPEND with type +'a fiber = 'a Lwt.t)
    (Conduit : Conduit.S
                 with type +'a io = 'a Lwt.t
                  and type input = Cstruct.t
                  and type output = Cstruct.t)
    (HTTP : HTTP)
    (Uid : UID)
    (Ref : Sigs.REF) : sig
  val fetch :
    resolvers:Conduit.resolvers ->
    (Uid.t, _, Uid.t * int ref * int64, 'g, Scheduler.t) Sigs.access
    * Uid.t Carton_lwt.Thin.light_load
    * Uid.t Carton_lwt.Thin.heavy_load ->
    (Uid.t, Uid.t * int ref * int64, 'g) Sigs.store ->
    endpoint ->
    ?version:[> `V1 ] ->
    ?capabilities:Smart.Capability.t list ->
    [ `All | `Some of Ref.t list | `None ] ->
    Pack.t ->
    Index.t ->
    src:Pack.uid ->
    dst:Pack.uid ->
    idx:Index.uid ->
    ( [ `Pack of Uid.t * (Ref.t * Uid.t) list | `Empty ],
      [> `Msg of string | `Exn of exn | `Not_found ] )
    result
    Lwt.t

  val push :
    resolvers:Conduit.resolvers ->
    (Uid.t, Ref.t, Uid.t Pck.t, 'g, Scheduler.t) Sigs.access
    * Uid.t Carton_lwt.Thin.light_load
    * Uid.t Carton_lwt.Thin.heavy_load ->
    (Uid.t, Uid.t Pck.t, 'g) Sigs.store ->
    endpoint ->
    ?version:[> `V1 ] ->
    ?capabilities:Smart.Capability.t list ->
    [ `Create of Ref.t | `Delete of Ref.t | `Update of Ref.t * Ref.t ] list ->
    (unit, [> `Msg of string | `Exn of exn | `Not_found ]) result Lwt.t
end
