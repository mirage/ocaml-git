module type ENTRY = sig
  type hash
  type kind = Normal | Everybody | Exec | Symlink | Gitlink

  val kind_to_perm :
    kind -> [`Normal | `Dir | `Exec | `Everybody | `Link | `Commit]

  val perm_of_kind :
    kind -> [`Normal | `Dir | `Exec | `Everybody | `Link | `Commit]

  val perm_to_kind : [`Normal | `Exec | `Everybody | `Link | `Commit] -> kind
  val kind_of_perm : [`Normal | `Exec | `Everybody | `Link | `Commit] -> kind
  val pp_kind : kind Fmt.t

  type time = {lsb32: int32; nsec: int32}

  val pp_time : time Fmt.t

  type 'e flag = {assume: bool; extend: 'e option; stage: int; length: int}

  val pp_flag : 'e Fmt.t -> 'e flag Fmt.t

  type extend = {reserved: bool; skip_worktree: bool; intent_to_add: bool}

  val pp_extend : extend Fmt.t

  type info =
    { ctime: time
    ; mtime: time
    ; dev: int32
    ; ino: int32
    ; mode: kind
    ; uid: int32
    ; gid: int32
    ; size: int32 }

  val pp_info : info Fmt.t

  type entry = {info: info; hash: hash; flag: extend flag; path: Git.Path.t}

  val pp_entry : entry Fmt.t

  type index = entry list

  val pp_index : index Fmt.t
end

module Entry (H : Digestif.S) : ENTRY with type hash = H.t

module Make
    (Store : Git.S)
    (FS : Git.FS)
    (Entry : ENTRY with type hash := Store.Hash.t) :
  sig
    module Store : Git.S
    module Entry : ENTRY with type hash := Store.Hash.t
    module FS : Git.FS
    module StringMap : Map.S with type key = string

    type 'entry elt = Blob of 'entry | Tree of 'entry elt StringMap.t

    and 'entry t = Root of 'entry elt StringMap.t

    val pp : Entry.entry t Fmt.t
    val of_entries : Entry.entry list -> Entry.entry t
    val to_entries : to_entry:(Git.Path.t -> 'a -> 'b) -> 'a t -> 'b list

    type error

    val pp_error : error Fmt.t

    type file = [`Everybody | `Exec | `Normal]
    type link = [`Commit | `Link]
    type directory = [`Dir]

    val int_of_perm : file -> int

    val entry_of_stats :
      Store.Hash.t -> Git.Path.t -> ?gitlink:bool -> Unix.stats -> Entry.entry

    val write_blob :
         root:Fpath.t
      -> FS.t
      -> Git.Path.t
      -> Store.Value.Tree.perm * Cstruct.t
      -> (Entry.entry, error) result Lwt.t

    val write_blob_entry :
         Store.t
      -> FS.t
      -> Git.Path.t
      -> Entry.entry
      -> (Entry.entry, error) result Lwt.t

    val write_tree :
      root:Fpath.t -> FS.t -> Git.Path.t -> (unit, error) result Lwt.t

    val load_entries :
         Store.t
      -> FS.t
      -> dtmp:Cstruct.t
      -> (Entry.entry list, error) result Lwt.t

    val store_entries :
         Store.t
      -> FS.t
      -> dtmp:Cstruct.t
      -> Entry.entry list
      -> (unit, error) result Lwt.t

    module Write : sig
      val write_on_store :
        Store.t -> Entry.entry list -> (Store.Hash.t, error) result Lwt.t

      val update_on_store :
        Store.t -> FS.t -> (Store.Hash.t, error) result Lwt.t
    end

    module Read : sig
      val of_tree : Store.t -> Store.Hash.t -> Entry.entry list Lwt.t
    end

    module Snapshot : sig
      val of_tree :
           Store.t
        -> FS.t
        -> Store.Hash.t
        -> (Entry.entry list, error) result Lwt.t

      val of_commit :
           Store.t
        -> FS.t
        -> Store.Hash.t
        -> (Entry.entry list, error) result Lwt.t

      val of_reference :
           Store.t
        -> FS.t
        -> Store.Reference.t
        -> (Entry.entry list, error) result Lwt.t
    end
  end
  with module Store := Store
   and module FS := FS
   and module Entry := Entry
