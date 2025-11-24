{ nixSearch, ... }:
{ config, pkgs, ... }:

let nix-search = nixSearch.packages.${pkgs.stdenv.hostPlatform.system}.default;
in {
  home.packages = [
    nix-search
    pkgs.manix # CLI docs for nix
  ];

  home.activation.indexNixSearch =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      ${nix-search}/bin/nix-search
    '';
}
