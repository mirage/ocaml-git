type error = |

let pp_error : error Fmt.t = fun _ppf -> function _ -> .

let writer buf str =
  Buffer.add_string buf str;
  String.length str

let get ~resolvers:_ ?headers:_ uri =
  let connection = Curl.init () in
  let buf = Buffer.create 0x1000 in
  Curl.set_url connection (Uri.to_string uri);
  Curl.set_writefunction connection (writer buf);
  let open Lwt.Infix in
  Curl_lwt.perform connection >>= fun _ ->
  Curl.cleanup connection;
  let v = Buffer.contents buf in
  Lwt.return_ok ((), v)

let post ~resolvers:_ ?headers:_ uri contents =
  let connection = Curl.init () in
  let buf = Buffer.create 0x1000 in
  let pos = ref 0 in
  Curl.set_url connection (Uri.to_string uri);
  Curl.set_post connection true;
  Curl.set_httpheader connection
    [
      "content-type: applicaton/x-git-upload-pack-request";
      Fmt.strf "content-length: %d" (String.length contents);
    ];
  Curl.set_readfunction connection (fun len ->
      let len = min len (String.length contents - !pos) in
      let res = String.sub contents !pos len in
      pos := !pos + len;
      res);
  Curl.set_writefunction connection (fun str ->
      Buffer.add_string buf str;
      String.length str);
  let open Lwt.Infix in
  Curl_lwt.perform connection >>= fun _ ->
  Curl.cleanup connection;
  let v = Buffer.contents buf in
  Lwt.return_ok ((), v)
