{ lib, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  pname = "gitblame";
  version = "0.0.0";

  src = ./.;

  vendorSha256 = "sha256-2jLH0NX9FeONipEe/hkt40I8GMRsEjGQkxmJk3y6l5c=";
}
