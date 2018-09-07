.PHONY: all test clean

all:
	dune build

test:
	dune runtest -j 1

clean:
	dune clean
