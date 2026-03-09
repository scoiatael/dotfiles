{
  config,
  lib,
  pkgs,
  lix-module,
  ...
}:

{
  imports = [
    ../../modules/darwin/default.nix
    ../../modules/darwin/aerospace.nix
    ../../modules/darwin/sketchybar.nix
    ../../modules/darwin/air.nix
    ../../modules/darwin/lix.nix
    lix-module.darwinModules.default
  ];

  system.primaryUser = "lukaszczaplinski";
  ids.gids.nixbld = lib.mkForce 30000;
  networking.hostName = "LsAir";
}
