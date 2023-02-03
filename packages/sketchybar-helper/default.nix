{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
stdenv.mkDerivation {
  pname = "sketchybar-helper";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [ gnumake clang ];
  buildInputs = [ ];

  buildPhase = ''
    make
  '';

  installPhase = ''
    mkdir -p $out/bin
    mv helper $out/bin/sketchybar-helper
  '';
}
