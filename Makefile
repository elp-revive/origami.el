SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

PKG-FILES := origami.el

TEST-FILES := $(shell ls test/origami.el-*.el)

.PHONY: clean checkdoc lint install compile unix-test

ci: clean compile install

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile

unix-test:
	@echo "Testing..."
	$(EASK) exec ert-runner -L . $(LOAD-TEST-FILES) -t '!no-win' -t '!org'

clean:
	rm -rf .cask *.elc
