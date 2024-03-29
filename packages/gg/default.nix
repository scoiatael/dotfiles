{ lib, fetchFromGitHub, perl, iconv, openssl, pkg-config, rustPlatform
, webkitgtk, buildNpmPackage }:

let

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

    packageJSON = "${src}/package.json";

    dontInstall = true;
  };

in rustPlatform.buildRustPackage {
  inherit version src pname;

  sourceRoot = "${src.name}/src-tauri";

  cargoLock = {
    lockFile = "${src}/src-tauri/Cargo.lock";
    outputHashes = {
      "muda-0.11.5" = "sha256-z83C6772RgA8i5Cuzpw16ZcA4Gxop9IdQqpUuxoNOuA=";
    };
  };

  nativeBuildInputs = [
    pkg-config
    # for openssl-sys
    perl
  ];
  buildInputs = [ openssl iconv ];

  checkFlags = [ ];
}
