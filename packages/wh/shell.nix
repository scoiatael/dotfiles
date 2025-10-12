{ pkgs ? import <nixpkgs> { } }:

with pkgs;

mkShell {
  buildInputs = with pkgs; [
    graalvmPackages.graalvm-ce
    clojure
    watchexec
    age
    zlib
  ];
}
