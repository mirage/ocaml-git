# OPAM packages needed to build tests.
OPAM_PACKAGES="camlzip dolog core_kernel cryptokit uri \
               cmdliner sha mstruct re ocamlgraph \
               alcotest lwt conduit uri mirage-fs-unix io-page"

ppa=avsm/ocaml41+opam11
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1

opam init git://github.com/ocaml/opam-repository >/dev/null 2>&1
opam install ${OPAM_PACKAGES}

eval `opam config env`
ocaml setup.ml -configure --enable-tests --enable-mirage
make
make test
