## ocaml-git -- Git format and protocol in pure OCaml

Support for on-disk and in-memory Git stores. Can read and write all
the Git objects: the usual blobs, trees, commits and tags but also
the pack files, pack indexes and the index file (where the staging area
lives).

All the objects share a consistent API, and convenience functions are
provided to manipulate the different objects. For instance, it is
possible to make a pack file position independent (as the Zlib
compression might change the relative offsets between the packed
objects), to generate pack indexes from pack files, or to expand
the filesystem of a given commit.

The library comes with some command-line tools called `ogit-*` as a Proof-of-concept
of the core library which shares a similar interface with `git`, but where all
operations are mapped to the API exposed `ocaml-git` (and hence using only OCaml
code). However, these tools are not done to be use. They are just examples of how to use `ocaml-git`.

The API documentation is available
[online](http://mirage.github.io/ocaml-git/).

[![Build Status](https://travis-ci.org/mirage/ocaml-git.svg?branch=master)](https://travis-ci.org/mirage/ocaml-git)

### Build and Install Instructions

To build and install the project, simply run:
```
$ opam install git
```

### What is supported

* The loose object files can be read and written;
  - [blobs](http://mirage.github.io/ocaml-git/git/Git/Blob/index.html) (files)
  - [trees](http://mirage.github.io/ocaml-git/git/Git/Tree/index.html) (directories)
  - [commits](http://mirage.github.io/ocaml-git/git/Git/Commit/index.html) (revision history)
  - [tags](http://mirage.github.io/ocaml-git/git/Git/Tag/index.html) (annotated tags)
  - [references](http://mirage.github.io/ocaml-git/git/Git/Reference/index.html) (branch names)

* The [pack files](http://mirage.github.io/ocaml-git/git/Git/Pack/index.html)
  (collections of compressed loose objects using a binary-diff representation)
  and [pack indexes](http://mirage.github.io/ocaml-git/git/Git/Pack_index/index.html)
  (indexes of pack files) can be read and
  written). The binary diff hunks are exposed using a high-level
  position-independent representation so that they can be manipulated
  more easily. Pack file can be created and is compressed.

* The [index file](http://mirage.github.io/ocaml-git/git/Git/Index/index.html)
  (used as for managing the staging area)
  are fully supported. Which means that `git diff` and `git status`
  will work as expected on a repository created by the library.

* [Cloning and fetching](http://mirage.github.io/ocaml-git/git/Git/Sync/index.html)
  (using various options) are fully supported for
  the Git protocol, the smart-HTTP protocol and `git+ssh`. A subset
  of the protocol capabilities are implemented (mainly `thin-pack`,
  `ofs-delta`, `side-band-64k` and `allow-reachable-sha1-in-want`).

* Pushing is still experimental and needs more testing.

* An abstraction for Git [Store](http://mirage.github.io/ocaml-git/git/Git/module-type-S/index.html)
  Is available. Various store implementations are available:
  - An [in-memory](http://mirage.github.io/ocaml-git/git/Git/Mem/Store/index.html) implementation;
  - A [unix filesystem](http://mirage.github.io/ocaml-git/git-unix/Git_unix/Store/index.html)
    implementation;
  - A [mirageOS](http://mirage.github.io/ocaml-git/git-mirage/Git_mirage/Store/index.html) implementation,
    requiring a `Mirage_fs_lwt` implementation.

### What is *not* supported

* No server-side operations are currently supported.
* No GC.
* Updates, merge and rebase are not supported. Use
  [irmin](https://github.com/mirage/irmin) instead.

### Performance

Performance is comparable to the Git tool.

### Example

```ocaml
# #require "git.unix";;
# open Git_unix;;
# module Search = Git.Search.Make(FS);;

# let read filename =
    let open Lwt_result.Infix in
    Store.v (Fpath.v ".") >>= fun t ->
    Store.Ref.resolve Store.Reference.master >>= fun head ->
    let open Lwt.Infix in
    Search.find t head (`Commit (`Path filename)) >|= function
    | None -> Lwt.return (Error `Not_found)
    | Some hash -> Store.read t hash
val read : string list -> (Store.Value.t, Store.error) Lwt_result.t
    
# let pp =
    let ok ppf = function
      | Store.Value.Blob blob ->
        Fmt.string ppf (Store.Value.Blob.to_string blob)
      | _ -> Fmt.string ppf "#git-object" in
    Fmt.pp_result ~ok ~error:Store.pp_error
val pp : (Store.Value.t, Store.error) Fmt.t

# Lwt_main.run Lwt.Infix.(read [ "README.md" ] >|= pp Fmt.stdout)

ocaml-git -- Git format and protocol in pure OCaml

Support for on-disk and in-memory Git stores. Can read and write all
the Git objects: the usual blobs, trees, commits and tags but also
the pack files, pack indexes and the index file (where the staging area
lives).

[...]
```

### License

MIT, see [LICENSE.md] file for its text.

[LICENSE.md]: ./LICENSE.md
