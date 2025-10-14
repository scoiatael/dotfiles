{ pkgs ? import <nixpkgs> { }, ... }:

let
  inherit (pkgs) lib;
  runFile = pkgs.writeShellApplication {
    name = "ip";
    text = ''
      cd "$(dirname "$0")/../" || exit 1
      ${lib.getExe pkgs.babashka} server.clj
    '';
  };
in pkgs.stdenv.mkDerivation {
  name = "ip";
  src = ./.;
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];

  buildPhase = ''
    mkdir -p $out/bin
    cp -r $src/* $out/
    cp ${pkgs.lib.getExe runFile} $out/bin/ip
  '';
}
