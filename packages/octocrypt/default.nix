{ pkgs ? import <nixpkgs> { }, ... }:

let
  runFile = pkgs.writeShellApplication {
    name = "octocrypt";
    runtimeInputs = with pkgs; [ age gnupg clojure ];
    text = ''
      cd "$(dirname "$0")/../" || exit 1
      clj -Srepro -J-Djava.util.logging.manager=org.apache.logging.log4j.jul.LogManager -X octocrypt.server/main
    '';
  };
in pkgs.stdenv.mkDerivation {
  name = "octocrypt";
  src = ./.;
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];

  buildPhase = ''
    mkdir -p $out/bin
    cp -r $src/src $out/src
    cp -r $src/public $out/public
    cp -r $src/deps.edn $out/deps.edn
    cp ${pkgs.lib.getExe runFile} $out/bin/octocrypt
  '';

}
