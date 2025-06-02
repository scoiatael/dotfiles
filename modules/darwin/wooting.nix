{ lib, pkgs, ... }:

{
  # hardware.notch = false;
  homebrew = {
    casks = [
      "bitwarden"
      "arc"
      "wezterm"
      "slack"
      "zed"
      "google-drive"
      "monodraw"
      "altair-graphql-client"
    ];
    brews = [ "stlink" ];
  };
  nix.linux-builder.enable = true;
  nix.settings.trusted-users = [ "@admin" ];
  # services.aerospace.settings.gaps.outer.top = lib.mkForce 42;
}
