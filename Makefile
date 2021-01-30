SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

ORIGAMI-FILES := $(wildcard ./origami-*.el)

TEST-FILES := test/bootstrap.el $(shell ls test/origami-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

build:
    EMACS=$(EMACS) cask install
    EMACS=$(EMACS) cask build
    EMACS=$(EMACS) cask clean-elc

ci: CASK=
ci: compile clean

compile:
    @echo "Compiling..."
    @$(CASK) $(EMACS) -Q --batch \
        -l test/bootstrap.el \
        -L . -L clients \
        --eval '(setq byte-compile-error-on-warn t)' \
        -f batch-byte-compile $(ORIGAMI-FILES)

show:
    la

clean:
    rm -rf .cask *.elc

.PHONY: clean build ci compile show
