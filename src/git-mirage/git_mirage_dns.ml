module Make
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S)
    (Stack : Mirage_stack.V4) (TCP : sig
      val tcp_ipaddr : Ipaddr.V4.t Mimic.value
    end) =
struct
  open Lwt.Infix
  include Dns_client_mirage.Make (Random) (Time) (Mclock) (Stack)

  let dns = Mimic.make ~name:"dns"

  let with_dns ?size ?nameserver ?timeout stack ctx =
    let v = create ?size ?nameserver ?timeout stack in
    Mimic.add dns v ctx

  let ctx =
    let k dns hostname =
      match dns, hostname with
      | _, `Name _ -> Lwt.return_none
      | _, `Addr (Ipaddr.V4 ipv4) -> Lwt.return_some ipv4
      | _, `Addr (Ipaddr.V6 _) -> Lwt.return_none (* TODO *)
      | None, `Domain _ -> Lwt.return_none
      | Some dns, `Domain domain_name -> (
          gethostbyname dns domain_name >>= function
          | Ok ipv4 -> Lwt.return_some ipv4
          | _ -> Lwt.return_none)
    in
    let open Mimic in
    fold TCP.tcp_ipaddr Fun.[ opt dns; req Smart_git.git_host ] ~k Mimic.empty
end
