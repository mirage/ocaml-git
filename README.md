## ocaml-git

Pure OCaml low-level bindings to Git -- Guaranteed no C inside.

Support for on-disk and in-memory Git stores. Can read and write all
the Git objects: the usual blobs, trees, commits and tags but also the
pack files, pack indexes and the index file (where the staging area
lives).

All the objects share a consistent API, and convenience functions are
provided to manipulate the different objects. For instance, it is
possible to make a pack file position independant (as the Zlib
compression might change the relative offsets between the packed
objects), to generate pack indexes from pack files, or to expand
the filesystem of a given commit.

[![Build Status](https://travis-ci.org/samoht/ocaml-git.png?branch=master)](https://travis-ci.org/samoht/ocaml-git)

### Build and Install Instructions

To build the project, simply run:
```
$ opam install camlzip dolog core_kernel cryptokit uri \
               cmdliner lazy-trie mstruct re ocamlgraph lwt \
               alcotest
$ make
```

Then to install it:
```
$ make install
```

### What is supported

* The loose object files can be read and written;
* The pack files can be read but not written;
* Cloning and fetching works for remote repository --
  no support for local clone, shallow objects and
  capacities yet (this means that we are not using
  the special optimization available in the protocol
  but the operations should be safe);
* Index file: no support yet -- this means that `git status`
   will think that you have no staged all the files in the
   directory.
* No pull/merge/rebase (and merging strategy)

### It should be painfully slow

Well, actually, not really. Performance is comparable to the
usual `git` command.

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
