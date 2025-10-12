{ pkgs ? import <nixpkgs> { }, ... }:

let
  clojure = pkgs.clojure.override { jdk = pkgs.graalvmPackages.graalvm-ce; };
  runFile = pkgs.writeShellApplication {
    name = "wh";
    runtimeInputs = [ clojure pkgs.age ];
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
    cp -r $src/build.clj $out/build.clj
    cp ${pkgs.lib.getExe runFile} $out/bin/wh
  '';
}
