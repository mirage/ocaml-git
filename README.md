## ocaml-git -- Git format and protocol in pure OCaml

Support for on-disk and in-memory Git stores. Can read and write all the Git
objects: the usual blobs, trees, commits and tags but also the pack files, pack
indexes and the index file (where the staging area lives - only for `git-unix`
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
high-level processes such as a ([patience][patience-diff]) diff, `git status`,
etc. are not implemented.

As a MirageOS project, `ocaml-git` is system agnostic. However, it provides a
`git-unix` package which uses UNIX _syscall_ and it able to introspect a Git
repository as you usually know. However, `ocaml-git` handles only Git objects
and it does not _populate_ your filesystem as `git` does.

For example, `Git_unix.Sync.clone` does not give you files of your repository
but synchronize your `.git` with your repository.

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
  supported. Which means that `git diff` and `git status` will work as expected
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
# #require "checkseum.c" ;;
# #require "digestif.c" ;;
# #require "git-unix" ;;
# open Git_unix ;;
# module Search = Git.Search.Make(Store) ;;

# let read filename =
    let open Lwt_result.Infix in
    Store.v (Fpath.v ".") >>= fun t ->
    Store.Ref.resolve t Store.Reference.master >>= fun head ->
    let open Lwt.Infix in
    Search.find t head (`Commit (`Path filename)) >>= function
    | None -> Lwt.return (Error `Not_found)
    | Some hash -> Store.read t hash
;;
val read : string list -> (Store.Value.t, Store.error) Lwt_result.t
    
# let pp =
    let ok ppf = function
      | Store.Value.Blob blob ->
        Fmt.string ppf (Store.Value.Blob.to_string blob)
      | _ -> Fmt.string ppf "#git-object" in
    Fmt.result ~ok ~error:Store.pp_error
;;
val pp : (Store.Value.t, Store.error) Fmt.t

# Lwt_main.run Lwt.Infix.(read [ "README.md" ] >|= pp Fmt.stdout) ;;

ocaml-git -- Git format and protocol in pure OCaml

Support for on-disk and in-memory Git stores. Can read and write all
the Git objects: the usual blobs, trees, commits and tags but also
the pack files, pack indexes and the index file (where the staging area
lives).

[...]
```

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
