{ config, lib, pkgs, ... }:

{
  services.aerospace = {
    enable = true;
    settings =
      builtins.fromTOML (builtins.readFile ../../config/aerospace.toml);
  };
  # TODO: https://nikitabobko.github.io/AeroSpace/goodness#show-aerospace-workspaces-in-sketchybar
  # TODO: https://www.raycast.com/limonkufu/aerospace
}
