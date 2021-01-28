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
  val create : mode:'a mode -> t -> uid -> ('a fd, error) result fiber
  val map : t -> 'm rd fd -> pos:int64 -> int -> Bigstringaf.t fiber
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
        | `HTTPS of (string * string) list ];
      path : string;
      host : [ `Addr of Ipaddr.t | `Domain of [ `host ] Domain_name.t ];
    }

    val pp : t Fmt.t
    val of_string : string -> (t, [> `Msg of string ]) result

    val with_headers_if_http : (string * string) list -> t -> t
    (** [with_headers_if_http hdrs edn] if endpoint [edn] is [`HTTP]  or [`HTTPS]
        adds [hdrs] to [edn] *)
  end

  val git_capabilities : [ `Rd | `Wr ] Mimic.value

  module Make
      (Scheduler : Sigs.SCHED with type +'a s = 'a Lwt.t)
      (Pack : APPEND with type +'a fiber = 'a Lwt.t)
      (Index : APPEND with type +'a fiber = 'a Lwt.t)
      (HTTP : HTTP)
      (Uid : UID)
      (Ref : Sigs.REF) : sig
    val fetch :
      ?push_stdout:(string -> unit) ->
      ?push_stderr:(string -> unit) ->
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
