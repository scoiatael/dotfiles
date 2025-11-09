{ pkgs ? import <nixpkgs> { }, ... }:

pkgs.buildGoModule {
  pname = "ip";
  version = "0.1.0";
  src = ./.;

  vendorHash = null;

  meta = with pkgs.lib; {
    description = "Simple HTTP server that returns the client's IP address";
    license = licenses.mit;
    mainProgram = "ip";
  };
}
