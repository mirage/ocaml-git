let blit0 src src_off dst dst_off len =
  let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
  Cstruct.blit src src_off dst 0 len

let blit1 src src_off dst dst_off len =
  let src = Cstruct.of_bigarray ~off:src_off ~len src in
  Cstruct.blit src 0 dst dst_off len

open Lwt.Infix
open Rresult

let ( >>? ) = Lwt_result.bind

module Make (Flow : Mirage_flow.S) = struct
  type +'a fiber = 'a Lwt.t

  type t = {
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    flow : Flow.flow;
  }

  type error = [ `Error of Flow.error | `Write_error of Flow.write_error ]

  let pp_error ppf = function
    | `Error err -> Flow.pp_error ppf err
    | `Write_error err -> Flow.pp_write_error ppf err

  let make flow = { flow; queue = Ke.Rke.create ~capacity:0x1000 Bigarray.char }

  let recv flow payload =
    if Ke.Rke.is_empty flow.queue then (
      Flow.read flow.flow >|= R.reword_error (fun err -> `Error err)
      >>? function
      | `Eof -> Lwt.return_ok `End_of_flow
      | `Data res ->
          Ke.Rke.N.push flow.queue ~blit:blit0 ~length:Cstruct.len res;
          let len = min (Cstruct.len payload) (Ke.Rke.length flow.queue) in
          Ke.Rke.N.keep_exn flow.queue ~blit:blit1 ~length:Cstruct.len ~off:0
            ~len payload;
          Ke.Rke.N.shift_exn flow.queue len;
          Lwt.return_ok (`Input len))
    else
      let len = min (Cstruct.len payload) (Ke.Rke.length flow.queue) in
      Ke.Rke.N.keep_exn flow.queue ~blit:blit1 ~length:Cstruct.len ~len payload;
      Ke.Rke.N.shift_exn flow.queue len;
      Lwt.return_ok (`Input len)

  let send flow payload =
    Flow.write flow.flow payload >|= function
    | Error `Closed -> R.error (`Write_error `Closed)
    | Error err -> R.error (`Write_error err)
    | Ok () -> R.ok (Cstruct.len payload)
end
