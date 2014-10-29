wget https://raw.githubusercontent.com/samoht/ocaml-travisci-skeleton/master/.travis-opam.sh
sh .travis-opam.sh

eval `opam config env`

./configure --enable-tests --disable-mirage
make
make test
make install
make uninstall

make clean
opam install mirage-types io-page ipaddr
./configure --enable-tests --enable-mirage
make
make test
make install
make uninstall
