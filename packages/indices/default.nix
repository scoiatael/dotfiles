{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "indices";
  version = "0.0.0";

  src = ./.;

  cargoHash = "sha256-B8WuYsm+3q5fvAy8LIDVnBnsxii68c8eSwrJYMHLFKA=";
}
