{ pkgs ? import <nixpkgs> { } }:

with pkgs;

buildGoModule {
  pname = "mvbak";
  version = "0.1.0";
  src = ./.;

  vendorHash = null;

  # Use just to build the binaries
  nativeBuildInputs = [ just ];

  # Skip the default build phase since we're using just
  dontBuild = true;

  # Install both binaries
  installPhase = ''
    just build
    mkdir -p $out/bin
    cp bin/mvbak/mvbak $out/bin/
    cp bin/unmvbak/unmvbak $out/bin/
  '';

  checkPhase = ''
    just test
  '';
}
