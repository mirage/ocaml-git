let domain_name = Alcotest.testable Domain_name.pp Domain_name.equal
let ipaddr = Alcotest.testable Ipaddr.pp (fun a b -> Ipaddr.compare a b = 0)
let ( <.> ) f g x = f (g x)
let github_com = Domain_name.(host_exn <.> of_string_exn) "github.com"
let private_network = Ipaddr.of_string_exn "10.0.0.0"

let test01 =
  Alcotest.test_case "edn01" `Quick @@ fun () ->
  match Smart_git.Endpoint.of_string "git@github.com:mirage/ocaml.git" with
  | Ok
      {
        Smart_git.Endpoint.scheme = `SSH "git";
        host = `Domain v;
        path = "mirage/ocaml.git";
      } ->
      Alcotest.(check domain_name) "github.com" v github_com
  | Ok v -> Alcotest.failf "Unexpected Git endpoint: %a" Smart_git.Endpoint.pp v
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %S" err

let test02 =
  Alcotest.test_case "edn02" `Quick @@ fun () ->
  match Smart_git.Endpoint.of_string "git@10.0.0.0:mirage/ocaml.git" with
  | Ok
      {
        Smart_git.Endpoint.scheme = `SSH "git";
        host = `Addr v;
        path = "mirage/ocaml.git";
      } ->
      Alcotest.(check ipaddr) "10.0.0.0" v private_network
  | Ok v -> Alcotest.failf "Unexpeted Git endpoint: %a" Smart_git.Endpoint.pp v
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %S" err

let test03 =
  Alcotest.test_case "edn03" `Quick @@ fun () ->
  match Smart_git.Endpoint.of_string "git@[10.0.0.0]:mirage/ocaml.git" with
  | Ok
      {
        Smart_git.Endpoint.scheme = `SSH "git";
        host = `Addr v;
        path = "mirage/ocaml.git";
      } ->
      Alcotest.(check ipaddr) "10.0.0.0" v private_network
  | Ok v -> Alcotest.failf "Unexpeted Git endpoint: %a" Smart_git.Endpoint.pp v
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %S" err

let test04 =
  Alcotest.test_case "edn04" `Quick @@ fun () ->
  match Smart_git.Endpoint.of_string "git://github.com/mirage/ocaml.git" with
  | Ok
      {
        Smart_git.Endpoint.scheme = `Git;
        host = `Domain v;
        path = "/mirage/ocaml.git";
      } ->
      Alcotest.(check domain_name) "github.com" v github_com
  | Ok v -> Alcotest.failf "Unexpeted Git endpoint: %a" Smart_git.Endpoint.pp v
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %S" err

let test05 =
  Alcotest.test_case "edn05" `Quick @@ fun () ->
  match Smart_git.Endpoint.of_string "http://github.com/mirage/ocaml.git" with
  | Ok
      {
        Smart_git.Endpoint.scheme = `HTTP [];
        host = `Domain v;
        path = "/mirage/ocaml.git";
      } ->
      Alcotest.(check domain_name) "github.com" v github_com
  | Ok v -> Alcotest.failf "Unexpeted Git endpoint: %a" Smart_git.Endpoint.pp v
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %S" err

let test06 =
  Alcotest.test_case "edn06" `Quick @@ fun () ->
  match Smart_git.Endpoint.of_string "http://10.0.0.0/mirage/ocaml.git" with
  | Ok
      {
        Smart_git.Endpoint.scheme = `HTTP [];
        host = `Addr v;
        path = "/mirage/ocaml.git";
      } ->
      Alcotest.(check ipaddr) "10.0.0.0" v private_network
  | Ok v -> Alcotest.failf "Unexpeted Git endpoint: %a" Smart_git.Endpoint.pp v
  | Error (`Msg err) -> Alcotest.failf "Unexpected error: %S" err

let () =
  Alcotest.run "smart git endpoint"
    [ "endpoint", [ test01; test02; test03; test04; test05; test06 ] ]
