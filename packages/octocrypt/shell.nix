{ pkgs ? import <nixpkgs> { }, ... }:

pkgs.mkShellNoCC { buildInputs = [ pkgs.clojure pkgs.watchexec pkgs.just pkgs.age pkgs.gnupg ]; }
