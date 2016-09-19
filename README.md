## ocaml-git

Pure OCaml Git format and protocol -- Guaranteed no C inside.

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

The library comes with a command-line tool called `ogit` which shares
a similar interface with `git`, but where all operations are mapped to
the API exposed `ocaml-git` (and hence using only OCaml code).

The API documentation is available
[online](http://mirage.github.io/ocaml-git/).

[![Build Status](https://travis-ci.org/mirage/ocaml-git.png?branch=master)](https://travis-ci.org/mirage/ocaml-git)

### Build and Install Instructions

To build and install the project, simply run:
```
$ opam install git
```

### What is supported

* The loose object files can be read and written;
  - [blobs](http://mirage.github.io/ocaml-git/Blob.html) (files)
  - [trees](http://mirage.github.io/ocaml-git/Tree.html) (directories)
  - [commits](http://mirage.github.io/ocaml-git/Commit.html) (revision history)
  - [tags](http://mirage.github.io/ocaml-git/Tag.html) (annotated tags)
  - [references](http://mirage.github.io/ocaml-git/Reference.html) (branch names)

* The [pack files](http://mirage.github.io/ocaml-git/Pack.html)
  (collections of compressed loose objects using a binary-diff representation)
  and [pack indexes](http://mirage.github.io/ocaml-git/Pack_index.html)
  (indexes of pack files) can be read and
  written). The binary diff hunks are exposed using a high-level
  position-independent representation so that they can be manipulated
  more easily. Pack file can be created but will not be compressed yet.

* The [index file] (https://github.com/samoht/ocaml-git/blob/master/lib/index.mli)
  (used as for managing the stagging area)
  are fully supported. Which means that `git diff` and `git status`
  will work as expected on a repository created by the library.

* [Cloning and fetching](http://mirage.github.io/ocaml-git/Sync.html)
  (using various options) are fully supported for
  the Git protocol, the smart-HTTP protocol and `git+ssh`. A subset
  of the protocol capabilities are implemented (mainly `thin-pack`,
  `ofs-delta`, `side-band-64k` and `allow-reachable-sha1-in-want`).

* Pushing is still experimental and need more testing.

* An abstraction for Git [Store](http://mirage.github.io/ocaml-git/Store.S.html)
  is available. Various store implementations are available:
  - An [in-memory](http://mirage.github.io/ocaml-git/Memory.html) implementation;
  - A [unix filesystem](http://mirage.github.io/ocaml-git/Git_unix.S.FS.html)
    implementation;
  - A [mirageOS](http://mirage.github.io/ocaml-git/Git_mirage.html) implementation,
    requiring an `V1_LWT.FS` implementation.

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
# open Lwt.Infix;;
# open Git_unix;;
# module Search = Git.Search.Make(FS);;

# let read file =
    FS.create () >>= fun t ->
    FS.read_reference_exn t Git.Reference.master >>= fun head ->
    Search.find t head (`Commit (`Path file)) >>= function
    | None     -> failwith "file not found"
    | Some sha -> FS.read_exn t sha >>= function
      | Git.Value.Blob b -> Lwt.return (Git.Blob.to_raw b)
      | _ -> failwith "not a valid path"
 ;;
 val read : string list -> string Lwt.t = <fun>

# Lwt_main.run (read ["README.md"] >|= print_string)

Pure OCaml Git format and protocol -- Guaranteed no C inside.

Support for on-disk and in-memory Git stores. Can read and write all
the Git objects: the usual blobs, trees, commits and tags but also
the pack files, pack indexes and the index file (where the staging area
lives).

[...]
```

### License

MIT, see LICENSE file for its text.
