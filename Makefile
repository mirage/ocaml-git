.PHONY: all test clean fmt

all:
	dune build

test:
	dune runtest -j 1

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote
