{ pkgs ? import (fetchTarball {
  name = "nixos-unstable-2022-08-09";
  url =
    "https://github.com/NixOS/nixpkgs/archive/12363fb6d89859a37cd7e27f85288599f13e49d9.tar.gz";
  sha256 = "sha256:1rjf0difaznpaq8aal3hc8dzk04x3pwz7pr8ak9svjnn3ysqwl89";
}) { } }:
pkgs.stdenv.mkDerivation rec {
  pname = "gtklock";
  version = "0.1.0";

  src = pkgs.fetchgit {
    url = "https://github.com/jovanlanik/gtklock";
    rev = "19e9e2451f5cd24e047e3c7ba7c7965791254e3b";
    sha256 = "sha256-ora5opyw2fctxCHgBLtYAncJZz0Wta/XFEOLmE40r9Y=";
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
