# OPAM packages needed to build tests.
OPAM_PACKAGES="camlzip dolog nocrypto uri cmdliner mstruct ocamlgraph lwt \
               alcotest conduit mirage-types io-pages ipaddr"

ppa=avsm/ocaml41+opam11
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam libgmp-dev
export OPAMYES=1

opam init git://github.com/ocaml/opam-repository >/dev/null 2>&1
opam install ${OPAM_PACKAGES}
opam list

eval `opam config env`
ls $(ocamlfind query mirage-types)
./configure --enable-tests --enable-mirage
make
make test
