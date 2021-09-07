module Make
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Pclock : Mirage_clock.PCLOCK)
    (Time : Mirage_time.S)
    (Stack : Mirage_stack.V4V6) (TCP : sig
      val tcp_ipaddr : Ipaddr.t Mimic.value
    end) =
struct
  open Lwt.Infix
  include Dns_client_mirage.Make (Random) (Time) (Mclock) (Pclock) (Stack)

  let dns = Mimic.make ~name:"dns"

  let with_dns ?size ?nameservers ?timeout stack ctx =
    let v = create ?size ?nameservers ?timeout stack in
    Mimic.add dns v ctx

  let ctx =
    let k dns hostname =
      match dns, hostname with
      | _, `Name _ -> Lwt.return_none
      | _, `Addr ipaddr -> Lwt.return_some ipaddr
      | None, `Domain _ -> Lwt.return_none
      | Some dns, `Domain domain_name -> (
          gethostbyname dns domain_name >>= function
          | Ok ipv4 -> Lwt.return_some (Ipaddr.V4 ipv4)
          | _ -> Lwt.return_none)
    in
    let open Mimic in
    fold TCP.tcp_ipaddr Fun.[ opt dns; req Smart_git.git_host ] ~k Mimic.empty
end
