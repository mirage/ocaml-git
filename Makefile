all:
	$(MAKE) core
	$(MAKE) http
	$(MAKE) mirage
	$(MAKE) unix

core:
	ocaml pkg/pkg.ml build -n git

http:
	ocaml pkg/pkg.ml build -n git-http

mirage:
	ocaml pkg/pkg.ml build -n git-mirage --tests true
	ocaml pkg/pkg.ml test

unix:
	ocaml pkg/pkg.ml build -n git-unix --tests true
	ocaml pkg/pkg.ml test

clean:
	rm -rf _build _tests
	ocaml pkg/pkg.ml clean -n git
	ocaml pkg/pkg.ml clean -n git-http
	ocaml pkg/pkg.ml clean -n git-mirage
	ocaml pkg/pkg.ml clean -n git-unix

REPO=../opam-repository
PACKAGES=$(REPO)/packages

# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -n $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

opam-pkg:
	$(MAKE) pkg-git
	$(MAKE) pkg-git-http
	$(MAKE) pkg-git-mirage
	$(MAKE) pkg-git-unix
