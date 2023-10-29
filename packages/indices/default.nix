{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "indices";
  version = "0.0.0";

  src = ./.;

  cargoHash = "sha256-+wyrXWiceYRtzvhkIbyvpLULc0zkFmqT2bxKP1URBgg=";
}
