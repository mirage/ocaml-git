module type S = sig
  include Mirage_flow.S

  type endpoint

  val connect : endpoint -> (flow, error) result Lwt.t
end
