{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell { buildInputs = with pkgs; [ clojure watchexec age ]; }
