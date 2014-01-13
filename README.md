## Cagit

Pure OCaml low-level bindings to Git -- Guaranteed no C inside.

### Build Instructions

```
opam install camlzip dolog core_kernel cryptokit uri \
  cmdliner lazy-trie mstruct re ocamlgraph lwt
make
```

### What is supported

* Binary format: all the loose objects can be read and write. The pack
  files can be read but not write.
* Protocol: basic clone and fetch works for the git protocol. No
  support for local clone, shallow objects and capacity yet.
* Index file: no support yet
* No pull/merge/rebase (and merging strategy)

### It should be painfully slow

Not really:

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

### Next

* Support for in-memory stores
* Backend for irminsule (or directly as a persitence store)
