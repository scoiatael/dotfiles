# Exposes flake apps under the name of each host / home for building with nh.
{
  den,
  lib,
  inputs,
  ...
}:
{

  flake.packages = lib.genAttrs lib.systems.flakeExposed (
    system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
    in

    (den.lib.nh.denPackages { fromFlake = true; } pkgs)
  );
}
