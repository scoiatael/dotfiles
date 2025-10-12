{ pkgs ? import <nixpkgs> { }, ... }:

let
  runFile = pkgs.writeShellApplication {
    name = "wh";
    runtimeInputs = with pkgs; [ clojure age ];
    text = ''
      cd "$(dirname "$0")/../" || exit 1
      clj -M -m wh.server
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
    cp -r $src/deps.edn $out/deps.edn
    cp ${pkgs.lib.getExe runFile} $out/bin/wh
  '';
}
