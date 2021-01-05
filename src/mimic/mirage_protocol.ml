module type S = sig
  include Mirage_flow.S

  type endpoint

  val connect : endpoint -> (flow, write_error) result Lwt.t
end
