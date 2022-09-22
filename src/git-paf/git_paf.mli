val git_paf_scheme : [ `HTTP | `HTTPS ] Mimic.value
val git_paf_port : int Mimic.value
val git_paf_hostname : string Mimic.value

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
