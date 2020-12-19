type flow = private ..

include
  Mirage_flow.S
    with type flow := flow
     and type error = [ `Msg of string | `Not_found ]

type ctx
type 'edn value

module Fun : sig
  type ('k, 'res) args =
    | [] : ('res, 'res) args
    | ( :: ) : 'a arg * ('k, 'res) args -> ('a -> 'k, 'res) args

  and 'v arg

  val req : 'a value -> 'a arg
  val opt : 'a value -> 'a option arg
  val dft : 'a value -> 'a -> 'a arg
  val map : ('k, 'a) args -> 'k -> 'a arg
end

val make : name:string -> 'edn value
val add : 'edn value -> 'edn -> ctx -> ctx
val fold : 'edn value -> ('k, 'edn option Lwt.t) Fun.args -> k:'k -> ctx -> ctx
val empty : ctx

type ('edn, 'flow) protocol

val register :
  name:string ->
  (module Mirage_protocol.S with type flow = 'flow and type endpoint = 'edn) ->
  'edn value * ('edn, 'flow) protocol

val resolve : ctx -> (flow, [> error ]) result Lwt.t
