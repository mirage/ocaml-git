include Git.FS
  with type error = [ `System of string ]
   and type Dir.error = [ `Stat of string
                        | `Unlink of string
                        | `Rmdir of string
                        | `Opendir of (string * Fpath.t)
                        | `Path of string
                        | `Getcwd of string
                        | `Mkdir of string ]
   and type File.lock = Lock.t
   and type File.error = [ `Open of string
                         | `Write of string
                         | `Read of string
                         | `Close of string
                         | `Stat of string
                         | `Rename of string
                         | `Unlink of string
                         | `Mkdir of string ]
   and type Mapper.error = [ `Stat of string
                           | `Close of string
                           | `Mmap of string
                           | `Open of string ]
