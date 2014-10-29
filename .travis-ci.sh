wget https://raw.githubusercontent.com/samoht/ocaml-travisci-skeleton/master/.travis-opam.sh
sh .travis-opam.sh

eval `opam config env`
prefix=`opam config var prefix`

./configure --prefix=$prefix --enable-tests --disable-mirage
make
make test
make install
make uninstall

make clean
opam install mirage-types io-page ipaddr -y
./configure --prefix=$prefix --enable-tests --enable-mirage
make
make test
make install
make uninstall
