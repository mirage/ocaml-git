module Make
    (Stack : Mirage_stack.V4) (TCP : sig
      val tcp_stack : Stack.t Mimic.value
      val tcp_ipaddr : Ipaddr.V4.t Mimic.value
      val tcp_port : int Mimic.value
      val tcp_endpoint : (Stack.t * Ipaddr.V4.t * int) Mimic.value
    end) =
struct
  let git_path = Mimic.make ~name:"git-path"
  let git_capabilities = Mimic.make ~name:"git-capabilities"
  let git_scheme = Mimic.make ~name:"git-scheme"
  let with_git_path v ctx = Mimic.add git_path v ctx
  let fetch ctx = Mimic.add git_capabilities `Rd ctx
  let push ctx = Mimic.add git_capabilities `Wr ctx
  let gri ctx = Mimic.add git_scheme `Git ctx
  let ssh ctx = Mimic.add git_scheme `SSH ctx
  let http ctx = Mimic.add git_scheme `HTTP ctx
  let https ctx = Mimic.add git_scheme `HTTPS ctx

  let with_resolv ctx =
    let k scheme stack ipaddr port _path _cap =
      match scheme with
      | `Git | `HTTP -> Lwt.return_some (stack, ipaddr, port)
      | _ -> Lwt.return_none
    in
    Mimic.(
      fold TCP.tcp_endpoint
        Fun.
          [
            req git_scheme; req TCP.tcp_stack; req TCP.tcp_ipaddr;
            dft TCP.tcp_port 9418; req git_path; dft git_capabilities `Rd;
          ]
        ~k ctx)

  let ctx = with_resolv Mimic.empty

  let with_smart_git_endpoint edn ctx =
    match Smart_git.Endpoint.of_string edn with
    | Ok { Smart_git.Endpoint.path; scheme = `SSH _; _ } ->
        ssh (with_git_path path ctx)
    | Ok { Smart_git.Endpoint.path; scheme = `Git; _ } ->
        gri (with_git_path path ctx)
    | Ok { Smart_git.Endpoint.path; scheme = `HTTP _; _ } ->
        http (with_git_path path ctx)
    | Ok { Smart_git.Endpoint.path; scheme = `HTTPS _; _ } ->
        https (with_git_path path ctx)
    | _ -> ctx
end
