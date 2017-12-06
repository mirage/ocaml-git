module type GAMMA = sig
  val current : Fpath.t
  val temp    : Fpath.t
end

module type S = sig
  include Mirage_fs_lwt.S
  val connect : unit -> t Lwt.t
  (* XXX: the constructor should not be there *)
end

module Make (Gamma: GAMMA) (FS: S): Git.FS
  with type error = [ `System of string ]
   and type File.lock = Git.Mem.Lock.elt
   and type File.error = [ `Stat of string
                         | Mirage_fs.write_error
                         | Mirage_fs.error ]
   and type Dir.error = [ `Destroy of string
                        | `Listdir of string
                        | `Stat of string
                        | `Path of string
                        | Mirage_fs.write_error ]
   and type Mapper.error = Mirage_fs.error
     (* XXX(Jean Gabin): oui, je sais. *)
