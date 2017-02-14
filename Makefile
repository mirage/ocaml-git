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
