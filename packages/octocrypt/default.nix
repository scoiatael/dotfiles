{ pkgs ? import <nixpkgs> { }, ... }:

let
  runFile = pkgs.writeShellScript "octocrypt" ''
    cd $(dirname "$0")/../ || exit 1
    ${pkgs.clojure}/bin/clj -Srepro -J-Djava.util.logging.manager=org.apache.logging.log4j.jul.LogManager -X octocrypt.server/main
  '';
in pkgs.stdenv.mkDerivation {
  name = "octocrypt";
  src = ./.;
  text = "";
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];
  buildInputs = [ pkgs.age pkgs.gnupg ];

  buildPhase = ''
    mkdir -p $out/bin
    cp -r $src/src $out/src
    cp -r $src/public $out/public
    cp -r $src/deps.edn $out/deps.edn
    cp ${runFile} $out/bin/octocrypt
  '';

}
