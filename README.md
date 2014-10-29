## ocaml-git

Pure OCaml low-level bindings to Git -- Guaranteed no C inside.

Support for on-disk and in-memory Git stores. Can read and write all
the Git objects: the usual blobs, trees, commits and tags but also the
pack files, pack indexes and the index file (where the staging area
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

[![Build Status](https://travis-ci.org/samoht/ocaml-git.png?branch=master)](https://travis-ci.org/samoht/ocaml-git)

### Build and Install Instructions

To build and install the project, simply run:
```
$ opam install git
```

### What is supported

* The loose object files can be read and written;

- binary blobs (files)
- trees (directories)
- commits (revision history)

* The pack files and pack indexes (which are collections of compressed
  loose objects using a diff-based representation) can be read and
  written). Moreover, diff hunks are exposed using a higher-level
  position-independent representation so that they can be manipulated
  more easily.

* The index file (cache) -- used as for managing the stagging area --
  is fully supported. Which means that `git diff` and `git status`
  will work as expected on a repository created by the library.

* Basic support for client-side cloning and fetching (using bare and
 deepen options) but with no support for fancy download capabilities
 which is exposed by usual Git servers.

### What is *not* supported

* Only client-side operations are currently supported. Implementing
  the corresponding server-side implementation is high on the TODO
  list: using a dummy compression scheme, this should not be too hard
  to add, though.

* Merging and rebasing strategies are not supported.

### Performance

Performance is comparable to the `git` command. For instance, when
downloading and processing a 5.4MiB pack file (the download time
itself takes ~10s):

```
$ time ogit clone git://github.com/ocaml/opam-repository
Cloning into 'opam-repository' ...
Receiving data ... done.
Resolving deltas: 100% (37294/37294), done.
remote: Counting objects: 37294, done.
Checking out files: 100% (6527/6527), done.
HEAD is now at 267e08725291fc61b8411c0f20553ddd2e246d4f

real 0m22.522s
user 0m4.954s
sys  0m5.078s
```

```
$ time git clone git://github.com/ocaml/opam-repository
Cloning into 'opam-repository'...
remote: Reusing existing pack: 37868, done.
remote: Total 37868 (delta 0), reused 0 (delta 0)
Receiving objects: 100% (37868/37868), 5.41 MiB | 408.00 KiB/s, done.
Resolving deltas: 100% (13973/13973), done.
Checking connectivity... done

real	 0m17.409s
user	 0m1.198s
sys	 0m0.925s
```

Not much energy have been dedicated to profiling the protocol
implementation yet, so there is still plenty room for improvement.
