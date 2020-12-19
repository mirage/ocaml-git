module Make (Flow : Mirage_flow.S) : sig
  include Sigs.FLOW with type +'a fiber = 'a Lwt.t

  val make : Flow.flow -> t
end
