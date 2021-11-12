val git_paf_scheme : [ `HTTP | `HTTPS ] Mimic.value
val git_paf_port : int Mimic.value
val git_paf_hostname : string Mimic.value
val git_paf_sleep : (int64 -> unit Lwt.t) Mimic.value

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

(*
module Make
    (Time : Mirage_time.S)
    (Pclock : Mirage_clock.PCLOCK)
    (Stack : Mirage_stack.V4V6) (TCP : sig
      val tcp_endpoint : (Stack.t * Ipaddr.t * int) Mimic.value
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.t Mimic.value
    end) : sig
  val ctx : Mimic.ctx
end
*)
