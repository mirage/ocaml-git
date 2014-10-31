VERSION = $(shell grep 'Version:' _oasis | sed 's/Version: *//')
VFILE   = lib/version.ml

SETUP = ocaml setup.ml

build: setup.data $(VFILE)
	$(SETUP) -build $(BUILDFLAGS)

doc: setup.data build
	$(SETUP) -doc $(DOCFLAGS)

test: setup.data build
	$(SETUP) -test $(TESTFLAGS)

all: $(VFILE)
	$(SETUP) -all $(ALLFLAGS)

install: setup.data
	$(SETUP) -install $(INSTALLFLAGS)

uninstall: setup.data
	$(SETUP) -uninstall $(UNINSTALLFLAGS)

reinstall: setup.data
	$(SETUP) -reinstall $(REINSTALLFLAGS)

clean:
	$(SETUP) -clean $(CLEANFLAGS)
	rm -f $(VFILE)

distclean:
	$(SETUP) -distclean $(DISTCLEANFLAGS)

setup.data:
	$(SETUP) -configure $(CONFIGUREFLAGS)

configure:
	$(SETUP) -configure $(CONFIGUREFLAGS)

.PHONY: build doc test all install uninstall reinstall clean distclean configure

# OASIS_STOP

$(VFILE): _oasis
	echo "let current = \"$(VERSION)\"" > $@

init-doc:
	mkdir -p gh-pages
	cd gh-pages
	git init
	git remote add origin git@github.com:mirage/ocaml-git.git
	git checkout gh-pages
	git pull

update-doc: doc
	cd gh-pages
	$(shell cp ../git.docdir/*.html .)
	git commit -a -m "Update docs"
	git push
