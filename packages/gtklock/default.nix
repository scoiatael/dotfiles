{ pkgs ? import (fetchTarball {
  name = "nixos-unstable-2022-08-09";
  url =
    "https://github.com/NixOS/nixpkgs/archive/12363fb6d89859a37cd7e27f85288599f13e49d9.tar.gz";
  sha256 = "sha256:1rjf0difaznpaq8aal3hc8dzk04x3pwz7pr8ak9svjnn3ysqwl89";
}) { } }:
pkgs.stdenv.mkDerivation rec {
  pname = "gtklock";
  version = "0.1.0";

  src = fetchTarball {
    url =
      "https://github.com/jovanlanik/gtklock/archive/9d48fdb92ae3e482142b92184ca52f680185bff8.tar.gz";
    sha256 = "1a8vx6k7wfdjyz4fz1xf76xdlzmx15b4g36ichj9995170q74cdq";
  };

  buildInputs = [ pkgs.gtk-layer-shell pkgs.gtk3 pkgs.linux-pam ];
  nativeBuildInputs = [ pkgs.gcc pkgs.scdoc pkgs.pkg-config ];

  buildPhase = ''
    make
  '';

  installPhase = ''
    DESTDIR=$out make install
  '';
}
