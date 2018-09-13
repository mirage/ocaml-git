include
  Git.Tcp.NET
  with type socket = Lwt_unix.file_descr
   and type endpoint = Git.Gri.t
