{ pkgs ? import <nixpkgs> { } }:

with pkgs;

stdenv.mkDerivation rec {
  pname = "batman";
  version = "2024.01.12";

  src = fetchFromGitHub {
    owner = "eth-p";
    repo = "bat-extras";
    rev = "3860f0f1481f1d0e117392030f55ef19cc018ee4";
    sha256 = "sha256-/FsVf6JTttjryYYlUXJzOckSDV4TTGAAqRytrTY8+Xw=";
  };

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = [ bash bat ];

  buildPhase = ''
    bash build.sh --minify=none batman
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/batman $out/bin/
    wrapProgram $out/bin/batman \
      --prefix PATH : ${lib.makeBinPath [ bat groff ]}
  '';

  meta = with lib; {
    description = "Read system manual pages using bat as the formatter";
    homepage = "https://github.com/eth-p/bat-extras";
    license = licenses.mit;
    mainProgram = "batman";
    platforms = platforms.unix;
  };
}
