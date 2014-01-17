## ocaml-git

Pure OCaml low-level bindings to Git -- Guaranteed no C inside.

Support for on-disk and in-memory Git stores.

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
$ time git clone https://github.com/mirage/cowabloga xxx1
Cloning into 'xxx1'...
remote: Reusing existing pack: 616, done.
remote: Total 616 (delta 0), reused 0 (delta 0)
Receiving objects: 100% (616/616), 629.00 KiB | 347.00 KiB/s, done.
Resolving deltas: 100% (383/383), done.
Checking connectivity... done

real	 0m3.348s
user	 0m0.123s
sys	 0m0.074s
```

```
$ time ogit clone https://github.com/mirage/cowabloga xxx2
https://github.com/mirage/cowabloga xxx2
Cloning into 'xxx2' ...
Receiving objects: 100% (616/616), done.
remote: Counting objects: 616, done.

real	0m2.850s
user	0m0.554s
sys	0m0.232s
```

Remark: currently, `ogit clone` will unpack all the loose objects.
This is pretty inefficient. The usual `git clone` simply download
the pack file and unpack it on-demand.
