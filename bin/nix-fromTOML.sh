#!/usr/bin/env sh

exec nix eval --expr "with builtins; fromTOML(readFile \"$(realpath "$1")\")" --impure
