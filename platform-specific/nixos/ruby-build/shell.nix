{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
        buildInputs = [
                autoconf
                automake
                bison
                libyaml
                zlib
                openssl
                libyaml
        ];
        nativeBuildInputs = [
                pkgconfig
        ];
}
