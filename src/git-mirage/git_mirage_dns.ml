module Make
    (Random : Mirage_random.S)
    (Mclock : Mirage_clock.MCLOCK)
    (Time : Mirage_time.S)
    (Stack : Mirage_stack.V4) (TCP : sig
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.V4.t Mimic.value
    end) =
struct
  include Dns_client_mirage.Make (Random) (Time) (Mclock) (Stack)

  let domain_name = Mimic.make ~name:"domain-name"
  let with_domain_name v ctx = Mimic.add domain_name v ctx

  let with_resolv ctx =
    let open Lwt.Infix in
    let k stack domain_name =
      let dns = create stack in
      gethostbyname dns domain_name >>= function
      | Ok ipv4 -> Lwt.return_some ipv4
      | _ -> Lwt.return_none
    in
    Mimic.(
      fold TCP.tcp_ipaddr Fun.[ req TCP.tcp_stack; req domain_name ] ~k ctx)

  let ctx = with_resolv Mimic.empty

  let with_smart_git_endpoint edn ctx =
    match Smart_git.Endpoint.of_string edn with
    | Ok { Smart_git.Endpoint.host = `Domain host; _ } ->
        with_domain_name host ctx
    | Ok { Smart_git.Endpoint.host = `Addr (Ipaddr.V4 v); _ } ->
        Mimic.add TCP.tcp_ipaddr v ctx
    | Ok { Smart_git.Endpoint.host = `Addr (Ipaddr.V6 _); _ } ->
        assert false (* TODO *)
    | _ -> ctx
end
