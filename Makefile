.PHONY: all test

all: build
	@

build: dist/setup
	obuild build

test: build
	obuild test

dist/setup: cagit.obuild
	obuild configure --enable-tests

clean:
	obuild clean