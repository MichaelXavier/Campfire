CABAL=cabal-dev
GHC_PKG=ghc-pkg

all: build

doc: configure
	$(CABAL) haddock

install: install_deps
	$(CABAL) install

uninstall:
	 $(GHC_PKG) unregister googleplus

build: configure install_deps
	$(CABAL) build

install_deps: campfire.cabal
	$(CABAL) install-deps

configure: campfire.cabal **/*.hs
	$(CABAL) configure

spec: configure_tests
	$(CABAL) build
	$(CABAL) test

configure_tests:
	$(CABAL) configure --enable-tests

clean:
	$(CABAL) clean
