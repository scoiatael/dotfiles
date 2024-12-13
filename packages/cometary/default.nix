{ lib, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  pname = "cometary";
  version = "1.7.0";

  src = fetchFromGitHub {
    owner = "usrme";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-a75F9xhvH8tMDMfk3cuYQg9OWjRZ4gxw7qp3pBmoTDc=";
  };

  vendorHash = "sha256-plrIHxPI2QJuWnP9VsF5UqeQaK9+KDn9nsmqDmoa8M0=";
}
