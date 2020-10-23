## ocaml-git -- Git format and protocol in pure OCaml

Support for on-disk and in-memory Git stores. Can read and write all the Git
objects: blobs, trees, commits and tags. It can also handle pack files, pack
indexes and index files (where the staging area lives - only for `git-unix`
package).

All the objects share a consistent API, and convenience functions are provided
to manipulate the different objects. For instance, it is possible to make a pack
file position independent (as the Zlib compression might change the relative
offsets between the packed objects), to generate pack indexes from pack files,
or to expand the filesystem of a given commit.

The library comes with some command-line tools called `ogit-*` as a
Proof-of-concept of the core library which shares a similar interface with
`git`, but where all operations are mapped to the API exposed by `ocaml-git` (and
hence using only OCaml code). However, these tools are not meant to be used. They
are just examples of how to use `ocaml-git`.

`ocaml-git` wants to be a low-level library for [irmin][irmin]. By this fact,
high-level commands such as a ([patience][patience-diff]) diff, `git status`,
etc. are not implemented.

As a MirageOS project, `ocaml-git` is system agnostic. However, it provides a
`git-unix` package which uses UNIX _syscall_ and is able to introspect a usual Git
repository in a filesystem. However, `ocaml-git` handles only Git objects
and does not _populate_ your filesystem as `git` does. For example, `Git_unix.Sync.fetch` 
does not give you files fetched from the repository but only synchronizes `.git` with 
that repository.

The API documentation is available [online][documentation].

[![Build Status](https://travis-ci.org/mirage/ocaml-git.svg?branch=master)](https://travis-ci.org/mirage/ocaml-git)

### Build, Install Instructions and Packages

To build and install the project, simply run:
```sh
$ opam install git
$ opam install git-unix
$ opam install git-mirage
```

### _Linking-trick_

`ocaml-git` uses 2 libraries with the linking-trick:
* [digestif][digestif]
* [checkseum][checkseum]

These libraries provide a C implementation and an OCaml implementation (mostly
to be compatible with `js_of_ocaml`). However, `utop` or any a build-system such
as `ocamlbuild` are not able to choose between these implementations. So, you
must __explicitely__ choose one.

These libraries use [virtual-library][virtual-library] available with `dune`. If
your build-system is `dune`, you should not have any problem about that where
`dune` is able to take the _default_ implementation of these libraries.

### What is supported

* The loose object files can be read and written;
  - [blobs][blob] (files)
  - [trees][tree] (directories)
  - [commits][commit] (revision history)
  - [tags][tag] (annotated tags)
  - [references][reference] (branch names)

* The [PACK files][pack-file] (collections of compressed loose objects using a
  binary-diff representation) and [PACK indexes][pack-index] (indexes of pack
  files) can be read and written). The binary diff hunks are exposed using a
  high-level position-independent representation so that they can be manipulated
  more easily. Pack file can be created and is compressed.

* The [INDEX file][index] (used as for managing the staging area) are fully
  supported, which means that `git diff` and `git status` will work as expected
  on a repository created by the library. This feature is only available for
  `git-unix` when it needs to introspect a file-system.

* [Cloning and fetching][git-sync] (using various options) are fully supported
  for the Git protocol, the smart-HTTP protocol and `git+ssh`. A subset of the
  protocol capabilities are implemented (mainly `thin-pack`, `ofs-delta`,
  `side-band-64k` and `allow-reachable-sha1-in-want`).

* Pushing is still experimental and needs more testing.

* An abstraction for Git [Store][git-store]
  Is available. Various store implementations are available:
  - An [in-memory][git-store-memory] implementation;
  - A [unix filesystem][git-store-unix] implementation;

### What is *not* supported

* No server-side operations are currently supported.
* No GC.
* Updates, merge and rebase are not supported. Use [irmin][irmin] instead.

### Performance

Performance is comparable to the Git tool.

### Example

This `utop` example must run into the `ocaml-git` repository when the given path
is `.`.

```ocaml
# ;; load necessary modules
# #require "checkseum.c" ;;
# #require "digestif.c" ;;
# #require "git-unix" ;;

# ;; we are going to use this project's local repository
# module Store = Git_unix.Store ;; 
module Store = Git_unix.Store

# ;; this module is useful for finding git objects in a git store
# module Search = Git.Search.Make (Digestif.SHA1) (Store) ;;
module Search :
  sig
    type hash = Store.hash
    type store = Store.t
    type pred =
        [ `Commit of hash
        | `Tag of string * hash
        | `Tree of string * hash
        | `Tree_root of hash ]
    val pred : store -> ?full:bool -> hash -> pred list Lwt.t
    type path =
        [ `Commit of path | `Path of string list | `Tag of string * path ]
    val mem : store -> hash -> path -> bool Lwt.t
    val find : store -> hash -> path -> hash option Lwt.t
  end

# ;; we want to read the contents of a blob under name [filename]
# let read filename =
  let open Lwt_result.Syntax in
  (* get store located in current root's .git folder *)
  let* store = Store.v (Fpath.v (Sys.getcwd ())) in
  (* find obj-id pointed at by master branch (reference) *)
  let* commit_id = Store.Ref.resolve store Git.Reference.master in
  let open Lwt.Syntax in
  (* find obj-id of of [filename] as a git blob *)
  let* blob_id = Search.find store commit_id (`Commit (`Path [ filename ])) in
  match blob_id with
  | None -> Lwt.return (Error (`Not_found commit_id))
  | Some hash ->
      (* read contents of the blob *)
      Store.read store hash
  ;;
val read : string -> (Store.Value.t, Store.error) Lwt_result.t = <fun>

# let pp =
  let ok ppf = function
    | Git.Value.Blob b -> Fmt.string ppf (Git.Blob.to_string b)
    | _ -> Fmt.string ppf "#git-object"
  in
  Fmt.result ~ok ~error:Store.pp_error;;
val pp : ('_weak1 Git.Value.t, Store.error) result Fmt.t = <fun>

# Lwt_main.run Lwt.Infix.(read "README.md" >|= pp Fmt.stdout) ;;

ocaml-git -- Git format and protocol in pure OCaml

Support for on-disk and in-memory Git stores. Can read and write all
the Git objects: the usual blobs, trees, commits and tags but also
the pack files, pack indexes and the index file (where the staging area
lives).

[...]
```

()

### License

MIT, see [LICENSE.md][LICENSE.md] file for its text.

[documentation]: http://mirage.github.io/ocaml-git/
[irmin]: https://github.com/mirage/irmin
[patience-diff]: https://git-scm.com/docs/git-diff
[digestif]: https://github.com/mirage/digestif
[checkseum]: https://github.com/mirage/checkseum
[virtual-library]: https://dune.readthedocs.io/en/stable/variants.html
[blob]: http://mirage.github.io/ocaml-git/git/Git/Blob/module-type-S/index.html
[tree]: http://mirage.github.io/ocaml-git/git/Git/Tree/module-type-S/index.html
[commit]: http://mirage.github.io/ocaml-git/git/Git/Commit/module-type-S/index.html
[tag]: http://mirage.github.io/ocaml-git/git/Git/Tag/module-type-S/index.html
[reference]: http://mirage.github.io/ocaml-git/git/Git/Reference/module-type-S/index.html
[pack-file]: http://mirage.github.io/ocaml-git/git/Git/Pack/index.html
[pack-index]: http://mirage.github.io/ocaml-git/git/Git/Index_pack/index.html
[index]: http://mirage.github.io/ocaml-git/git-unix/Git_unix/Index/index.html
[git-sync]: http://mirage.github.io/ocaml-git/git/Git/Sync/module-type-S/index.html
[git-store]: http://mirage.github.io/ocaml-git/git/Git/Store/index.html
[git-store-memory]: http://mirage.github.io/ocaml-git/git/Git/Mem/index.html
[git-store-unix]: http://mirage.github.io/ocaml-git/git-unix/Git_unix/Store/index.html
[LICENSE.md]: ./LICENSE.md
