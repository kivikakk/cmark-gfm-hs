C_SOURCES=$(wildcard cbits/*.*)
CMARK_DIR?=../cmark-gfm

build:
	cabal configure --enable-tests && cabal build

test:
	cabal test

prep:
	cabal install --enable-tests --only-dependencies

install:
	cabal install

clean:
	cabal clean

bench:
	cabal configure --enable-benchmarks && \
	cabal install --only-dependencies && \
	cabal build && \
	cabal bench

update-c-sources: $(C_SOURCES)

cbits/config.h: $(CMARK_DIR)/build/src/config.h
	cp $< $@

cbits/cmark-gfm_export.h: $(CMARK_DIR)/build/src/cmark-gfm_export.h
	cp $< $@

cbits/cmark-gfm_version.h: $(CMARK_DIR)/build/src/cmark-gfm_version.h
	cp $< $@

cbits/cmark-gfm-extensions_export.h: $(CMARK_DIR)/build/extensions/cmark-gfm-extensions_export.h
	cp $< $@

cbits/%: $(CMARK_DIR)/src/%
	cp $< $@

cbits/%: $(CMARK_DIR)/extensions/%
	cp $< $@

.PHONY: build prep install test clean bench update-c-sources
