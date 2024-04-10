{ lib, stdenv, darwin, fetchNpmDeps, nodePackages, fetchFromGitHub, gnused, perl
, iconv, openssl, pkg-config, rustPlatform, webkitgtk, buildNpmPackage }:

let

  nixpkgsWithEsBuild01912 = import (fetchTarball {
    name = "nixos-esbuild-01912";
    url =
      "https://github.com/NixOS/nixpkgs/archive/b60c5f00ad54fa71b363c559c5af12e09e40b8d7.tar.gz";
  }) { };

  pname = "gg";
  version = "unstable-0.15.3";

  src = fetchFromGitHub {
    owner = "gulbanana";
    repo = pname;
    rev = "ef0ab3ccbe083cf1cebcf1918e63cf49b88a6cf1";
    sha256 = "sha256-pn4FMXLQOs2HIOI/8kZwPstvDWDRfsfOWB6zox3o8FY=";
  };

  frontend-build = buildNpmPackage {
    inherit version src;
    pname = "gg-ui";

    ESBUILD_BINARY_PATH = "${nixpkgsWithEsBuild01912.esbuild}/bin/esbuild";

    # npmFlags = [ "--ignore-scripts" ];

    npmDepsHash = "sha256-1rMfChTgsIsIvt1UPEroIkGFZmYgXuS6PemMR342H3A=";

    postBuild = ''
      cp -r dist $out
    '';

    dontInstall = true;
  };

in rustPlatform.buildRustPackage {
  inherit version src pname;

  sourceRoot = "${src.name}/src-tauri";

  cargoLock = {
    lockFile = ./Cargo.lock;
    outputHashes = {
      "muda-0.11.5" = "sha256-z83C6772RgA8i5Cuzpw16ZcA4Gxop9IdQqpUuxoNOuA=";
    };
  };

  postConfigure = ''
    # Problem 1: duplication of 'cargo-vendor-dir' caused by relative path in
    #  https://github.com/NixOS/nixpkgs/blob/7f13fd4e7960fe2cefaba1048ea1885014311635/pkgs/build-support/rust/import-cargo-lock.nix#L234
    CARGO_VENDOR_DIR=$(realpath ../../cargo-vendor-dir)
    substituteInPlace ../../.cargo/config "cargo-vendor-dir" "$CARGO_VENDOR_DIR"
    substituteInPlace ../../cargo-vendor-dir/.cargo/config "cargo-vendor-dir" "$CARGO_VENDOR_DIR"

    # Problem 2: locked dependencies of jj-cli missing in main Cargo.lock
    rm ../../cargo-vendor-dir/jj-cli-0.15.1/Cargo.lock
  '';

  postPatch = ''
    # Problem 3: "optional" dependencies of jj-cli are missing in main Cargo.lock but fail build process
    # Needed to generate a new one with it and replace upstream one
    ln -sf ${./Cargo.lock} ./Cargo.lock
    ln -sf ${./Cargo.toml} ./Cargo.toml

    # copy the frontend static resources to final build directory
    cp -R ${frontend-build}/* ../dist/
  '';

  nativeBuildInputs = [
    pkg-config
    # for openssl-sys
    perl
  ];

  buildInputs = [ openssl iconv ] ++
    # Problem 4:
    # Darwin build requires:
    #   - Security/SecureTransport.h because of libgit2
    #   - AppKit, WebKit to link main executable
    (lib.lists.optionals (stdenv.isDarwin)
      (with darwin.apple_sdk.frameworks; [ Security AppKit WebKit ]));

  doCheck = false;
}
