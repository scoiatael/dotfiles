{ config, lib, pkgs, ... }:

pkgs.stdenv.mkDerivation {
  name = "human";

  src = pkgs.fetchgit {
    url = "git://z3bra.org/human";
    hash = "sha256-6tVgDGL9eWi4t8p5w7pyAZLhUzrKxKs5FYs7fnfVC3g=";
  };

  nativeBuildInputs = [ pkgs.gnumake ];

  buildPhase = ''
    make
  '';

  installPhase = ''
    mkdir -p $out/{bin,man}
    cp human  $out/bin/
    cp human.1  $out/man/
  '';
}
