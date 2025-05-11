{ nixSearch, ... }:
{ config, pkgs, ... }:

let nix-search = nixSearch.packages.${pkgs.system}.default;
in {
  home.packages = [ nix-search ];

  home.activation.indexNixSearch =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      ${nix-search}/bin/nix-search
    '';
}
