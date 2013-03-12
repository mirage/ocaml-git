all: dist/setup
	obuild build

dist/setup: cagit.obuild
	obuild configure

clean:
	obuild clean