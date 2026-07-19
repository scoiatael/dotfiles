{
  lib,
  inputs,
  ...
}:
{
  # Required to build kagimcp
  flake-file = {
    inputs = {
      uv2nix.url = "github:pyproject-nix/uv2nix";
      pyproject-build-systems.url = "github:pyproject-nix/build-system-pkgs";
      pyproject-nix.url = "github:pyproject-nix/pyproject.nix";
    };
  };
  flake.packages = lib.genAttrs lib.systems.flakeExposed (
    system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      tree = pkgs.lib.filesystem.packagesFromDirectoryRecursive {
        callPackage = lib.callPackageWith (pkgs // { inherit inputs; });
        directory = ../packages;
      };
    in
    lib.mapAttrs (_: { default, ... }: default) tree
  );
}
