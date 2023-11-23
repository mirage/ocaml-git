module type APPEND = sig
  type 'a rd = < rd : unit ; .. > as 'a
  type 'a wr = < wr : unit ; .. > as 'a

  type 'a mode =
    | Rd : < rd : unit > mode
    | Wr : < wr : unit > mode
    | RdWr : < rd : unit ; wr : unit > mode

  type t
  type uid
  type 'a fd
  type error
  type +'a fiber

  val pp_error : error Fmt.t

  val create :
    ?trunc:bool -> mode:'a mode -> t -> uid -> ('a fd, error) result fiber

  val map : t -> 'm rd fd -> pos:int64 -> int -> Bigstringaf.t
  val append : t -> 'm wr fd -> string -> unit fiber
  val move : t -> src:uid -> dst:uid -> (unit, error) result fiber
  val close : t -> 'm fd -> (unit, error) result fiber
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
    ctx:Mimic.ctx ->
    ?headers:(string * string) list ->
    Uri.t ->
    (unit * string, error) result Lwt.t

  val post :
    ctx:Mimic.ctx ->
    ?headers:(string * string) list ->
    Uri.t ->
    string ->
    (unit * string, error) result Lwt.t
end

module type SMART_GIT = sig
  module type APPEND = APPEND
  module type UID = UID
  module type HTTP = HTTP

  module Endpoint : sig
    type t = private {
      scheme :
        [ `SSH of string
        | `Git
        | `HTTP of (string * string) list
        | `HTTPS of (string * string) list
        | `Scheme of string ];
      port : int option;
      path : string;
      hostname : string;
    }

    val pp : t Fmt.t
    val of_string : string -> (t, [> `Msg of string ]) result

    val with_headers_if_http : (string * string) list -> t -> t
    (** [with_headers_if_http hdrs edn] if endpoint [edn] is [`HTTP]  or [`HTTPS]
        adds [hdrs] to [edn] *)

    val to_ctx : t -> Mimic.ctx -> Mimic.ctx
  end

  (** {3 Mimic values.}

      When the user use an [Endpoint.t] to {!Make.fetch} or {!Make.push}, we fill the given Mimic's [ctx]
      with some available informations such as:
      - if we want to {!Make.fetch} ([`Rd]) or {!Make.push} ([`Wr])
      - the scheme/protocol that the user would like to use ([git://], SSH or HTTP - with or without TLS)
      - the path of the git repository
      - the host (an IP adress or a domain name)
      - the SSH user iff the user would like to use SSH
      - the port that the user would like to use

      From this informations, the {b end}-user can process them through the Mimic API (with {!Mimic.fold})
      and describe how to create needed values to start {i a} protocol from them.

      For example, if the user wants to use [mirage-tcpip] which needs an IP address and a port,
      he is able to re-use/map/fold {!git_host} and {!git_port} to craft what [mirage-tcpip]
      really needs.

      Of course, such job is definitely outside the scope of [ocaml-git] and permits to us to be free
      about protocol implementations. An example of the plumbing needed is able with [git-mirage] which
      re-use these values to be able to start a [mirage-tcpip] connection, a [awa-ssh] connection
      of a [cohttp] (with or without [ocaml-tls]) connection. *)

  type handshake = uri0:Uri.t -> uri1:Uri.t -> Mimic.flow -> unit Lwt.t

  val git_capabilities : [ `Rd | `Wr ] Mimic.value

  val git_scheme :
    [ `Git | `SSH | `HTTP | `HTTPS | `Scheme of string ] Mimic.value

  val git_path : string Mimic.value
  val git_hostname : string Mimic.value
  val git_ssh_user : string Mimic.value
  val git_port : int Mimic.value
  val git_http_headers : (string * string) list Mimic.value

  val git_transmission :
    [ `Git | `Exec | `HTTP of Uri.t * handshake ] Mimic.value

  val git_uri : Uri.t Mimic.value

  module Make_client
      (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
      (Pack : APPEND with type +'a fiber = 'a Lwt.t)
      (Index : APPEND with type +'a fiber = 'a Lwt.t)
      (Uid : UID)
      (Ref : Sigs.REF) : sig
    val fetch :
      ?push_stdout:(string -> unit) ->
      ?push_stderr:(string -> unit) ->
      ?bounds:int ->
      ?threads:int ->
      ctx:Mimic.ctx ->
      (Uid.t, _, Uid.t * int ref * int64, 'g, Scheduler.t) Sigs.access
      * Uid.t Carton_lwt.Thin.light_load
      * Uid.t Carton_lwt.Thin.heavy_load ->
      (Uid.t, Uid.t * int ref * int64, 'g) Sigs.store ->
      Endpoint.t ->
      ?version:[> `V1 ] ->
      ?capabilities:Smart.Capability.t list ->
      ?deepen:[ `Depth of int | `Timestamp of int64 ] ->
      [ `All | `Some of Ref.t list | `None ] ->
      Pack.t ->
      Index.t ->
      src:Pack.uid ->
      dst:Pack.uid ->
      idx:Index.uid ->
      ( [ `Pack of Uid.t * (Ref.t * Uid.t) list | `Empty ],
        ([> `Exn of exn | Mimic.error ] as 'err) )
      result
      Lwt.t

    val push :
      ctx:Mimic.ctx ->
      (Uid.t, Ref.t, Uid.t Pck.t, 'g, Scheduler.t) Sigs.access
      * Uid.t Carton_lwt.Thin.light_load
      * Uid.t Carton_lwt.Thin.heavy_load ->
      (Uid.t, Uid.t Pck.t, 'g) Sigs.store ->
      Endpoint.t ->
      ?version:[> `V1 ] ->
      ?capabilities:Smart.Capability.t list ->
      [ `Create of Ref.t | `Delete of Ref.t | `Update of Ref.t * Ref.t ] list ->
      (unit, ([> `Exn of exn | Mimic.error ] as 'err)) result Lwt.t
  end

  module Make_server
      (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
      (Pack : APPEND with type +'a fiber = 'a Lwt.t)
      (Index : APPEND with type +'a fiber = 'a Lwt.t)
      (Uid : UID)
      (Ref : Sigs.REF) : sig
    val upload_pack :
      (Uid.t, _, _, 'g, Scheduler.t) Sigs.access
      * Uid.t Carton_lwt.Thin.light_load
      * Uid.t Carton_lwt.Thin.heavy_load ->
      (Uid.t, _, 'g) Sigs.store ->
      flow:Mimic.flow ->
      (* -> capabilities:Smart.Capability.t list empty list for now *)
      (unit, ([> `Exn of exn ] as 'err)) result Lwt.t
    (** Answers a [git fetch] *)

    val receive_pack :
      (Uid.t, _, Uid.t, 'g, Scheduler.t) Sigs.access
      * Uid.t Carton_lwt.Thin.light_load
      * Uid.t Carton_lwt.Thin.heavy_load ->
      (Uid.t, _, 'g) Sigs.store ->
      flow:Mimic.flow ->
      ?version:[> `V1 ] ->
      ?capabilities:Smart.Capability.t list ->
      Pack.t ->
      Index.t ->
      src:Pack.uid ->
      dst:Pack.uid ->
      idx:Index.uid ->
      (unit, ([> `Exn of exn ] as 'err)) result Lwt.t
    (** Answers a [git push]  *)

    val fetch :
      ?push_stdout:(string -> unit) ->
      ?push_stderr:(string -> unit) ->
      ?bounds:int ->
      ?threads:int ->
      ctx:Mimic.ctx ->
      (Uid.t, _, Uid.t * int ref * int64, 'g, Scheduler.t) Sigs.access
      * Uid.t Carton_lwt.Thin.light_load
      * Uid.t Carton_lwt.Thin.heavy_load ->
      (Uid.t, Uid.t * int ref * int64, 'g) Sigs.store ->
      Endpoint.t ->
      ?version:[> `V1 ] ->
      ?capabilities:Smart.Capability.t list ->
      ?deepen:[ `Depth of int | `Timestamp of int64 ] ->
      [ `All | `Some of Ref.t list | `None ] ->
      Pack.t ->
      Index.t ->
      src:Pack.uid ->
      dst:Pack.uid ->
      idx:Index.uid ->
      ( [ `Pack of Uid.t * (Ref.t * Uid.t) list | `Empty ],
        ([> `Exn of exn | Mimic.error ] as 'err) )
      result
      Lwt.t

    val push :
      ctx:Mimic.ctx ->
      (Uid.t, Ref.t, Uid.t Pck.t, 'g, Scheduler.t) Sigs.access
      * Uid.t Carton_lwt.Thin.light_load
      * Uid.t Carton_lwt.Thin.heavy_load ->
      (Uid.t, Uid.t Pck.t, 'g) Sigs.store ->
      Endpoint.t ->
      ?version:[> `V1 ] ->
      ?capabilities:Smart.Capability.t list ->
      [ `Create of Ref.t | `Delete of Ref.t | `Update of Ref.t * Ref.t ] list ->
      (unit, ([> `Exn of exn | Mimic.error ] as 'err)) result Lwt.t
  end
end
