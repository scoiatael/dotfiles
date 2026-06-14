{
  lib,
  inputs,
  ...
}:
{
  flake.packages = lib.genAttrs lib.systems.flakeExposed (
    system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      tree = pkgs.lib.filesystem.packagesFromDirectoryRecursive {
        inherit (pkgs) callPackage;
        directory = ../packages;
      };
    in
    lib.mapAttrs (_: { default, ... }: default) tree
  );
}
