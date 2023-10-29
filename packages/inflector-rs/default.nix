{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "inflector-rs";
  version = "0.0.0";

  src = ./.;

  cargoHash = "sha256-32/AVMQvkCkaYHjIpjOmxVmi7dQizhh5z6K445sm+A8=";
}
