{ inputs, lib, ... }:
let
  release = "26.05";
  mkNonFlakeInputs = lib.mapAttrs' (
    name:
    { url }:
    lib.nameValuePair name {
      flake = false;
      inherit url;
    }
  );
in
{
  # https://flake-file.denful.dev/guides/lock-flattening/
  imports = [ inputs.flake-file.flakeModules.nix-auto-follow ];
  flake-file = {
    inputs = {
      nix-index-database.url = "github:nix-community/nix-index-database";
      nixpkgs.url = "github:nixos/nixpkgs/nixos-${release}";
      home-manager.url = "github:nix-community/home-manager/release-${release}";
      darwin.url = "github:LnL7/nix-darwin/nix-darwin-${release}";
      stylix.url = "github:nix-community/stylix/release-${release}";
      nixos-hardware.url = "github:NixOS/nixos-hardware";
      lanzaboote.url = "github:nix-community/lanzaboote";
      walker.url = "github:abenz1267/walker";
      sops-nix.url = "github:Mic92/sops-nix";
      import-tree.url = "github:denful/import-tree";
      den.url = "github:denful/den";
      flake-file.url = "github:vic/flake-file";
      flake-parts.url = "github:hercules-ci/flake-parts";
    }
    // mkNonFlakeInputs {
      doomemacs.url = "github:doomemacs/doomemacs";
    };
  };
}
