# OPAM packages needed to build tests.
OPAM_PACKAGES="camlzip dolog core_kernel cryptokit uri \
               cmdliner lazy-trie mstruct re ocamlgraph lwt"

ppa=avsm/ocaml41+opam11
echo "yes" | sudo add-apt-repository ppa:$ppa
sudo apt-get update -qq
sudo apt-get install -qq ocaml ocaml-native-compilers camlp4-extra opam
export OPAMYES=1

opam init
opam install ${OPAM_PACKAGES}

eval `opam config env`
make
make test
