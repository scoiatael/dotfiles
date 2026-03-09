{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../../modules/darwin/default.nix
    ../../modules/darwin/aerospace.nix
    ../../modules/darwin/wooting.nix
    ../../modules/darwin/sketchybar.nix
    ../../modules/darwin/lix.nix
  ];

  system.primaryUser = "lukas";

  users.users.lukas = {
    name = "lukas";
    home = "/Users/lukas";
  };

  # Note: lix package is set via the lix module imported in flake.nix
}
