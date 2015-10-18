#!/bin/bash

# TODO:
# https://tkf.github.io/emacs-ipython-notebook/#quick-try

set -e

# Golang dependencies
GODEPS="golang.org/x/tools/cmd/goimports" \
      "github.com/nsf/gocode" \
      "github.com/dougm/goflymake" \
      "golang.org/x/tools/cmd/oracle"

if which go > /dev/null;
then
    for dep in $GODEPS;
    do
        go get -u -v $dep
    done
fi

# Ocaml dependencies
OCAML_DEPS="merlin"

if which opam > /dev/null;
then
    opam install $OCAML_DEPS
fi
