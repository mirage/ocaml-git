module Make (Flow : Mirage_flow.S) : sig
  include
    Sigs.FLOW
      with type +'a fiber = 'a Lwt.t
       and type error =
        [ `Error of Flow.error | `Write_error of Flow.write_error ]

  val make : Flow.flow -> t
end
