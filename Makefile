HTTP   ?= true
UNIX   ?= true
MIRAGE ?= true
TESTS  ?= true

OPTIONS=--with-http ${HTTP} --with-unix ${UNIX} --with-mirage ${MIRAGE} --tests ${TESTS}

all:
	ocaml pkg/pkg.ml build ${OPTIONS}
	ocaml pkg/pkg.ml test

clean:
	rm -rf _build _tests
	ocaml pkg/pkg.ml clean
