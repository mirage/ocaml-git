type endpoint = {
  uri    : Uri.t;
  headers: Cohttp.Header.t;
}

include Git.Tcp.NET
  with type socket = Lwt_unix.file_descr
   and type endpoint := endpoint
