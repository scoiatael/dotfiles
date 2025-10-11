{ pkgs ? import <nixpkgs> { }, ... }:

let
  runFile = pkgs.writeShellApplication {
    name = "wh";
    runtimeInputs = with pkgs; [ babashka clojure age ];
    text = ''
      cd "$(dirname "$0")/../" || exit 1
      bb -m wh.server
    '';
  };
in pkgs.stdenv.mkDerivation {
  name = "wh";
  src = ./.;
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];

  buildPhase = ''
    mkdir -p $out/bin
    cp -r $src/src $out/src
    cp -r $src/public $out/public
    cp -r $src/bb.edn $out/bb.edn
    cp ${pkgs.lib.getExe runFile} $out/bin/wh
  '';
}
