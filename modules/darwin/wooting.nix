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
      "forklift"
      "zen"
      "mitmproxy"
      "astropad-studio"
      "claude"
    ];
    brews = [ "stlink" ];
  };
  #  nix.linux-builder = {
  #   enable = true;
  #  systems = [ "x86_64-linux" "aarch64-linux" ];
  #  config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
  # };
  # nix.settings.trusted-users = [ "@admin" ];
  # services.aerospace.settings.gaps.outer.top = lib.mkForce 42;
}
