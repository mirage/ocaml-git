# Cagit

Pure OCaml low-level git bindings

**WARNING** this is work in progress and I'm not sure where this is
exactly going, but I certainly don't plan to implement all of the
libgit in OCaml.

### Build Instructions

```
opam install camlzip dolog core_kernel cryptokit uri \
  cmdliner lazy-trie mstruct re ocamlgraph
make
```
