{ lib, pkgs, ... }:

{
  # hardware.notch = false;
  homebrew = {
    casks = [
      "bitwarden"
      "slack"
      "google-drive"
      "monodraw"
      "altair-graphql-client"
      "forklift"
      "mitmproxy"
      "astropad-studio"
      "utm"
      "notion"
      "thebrowsercompany-dia"
    ];
    brews = [ "stlink" ];
  };
  nix.linux-builder = {
    enable = true;
    systems = [ "x86_64-linux" ];
    package = pkgs.darwin.linux-builder-x86_64;
  };
  #  nix.linux-builder = {
  #   enable = true;
  #  systems = [ "x86_64-linux" "aarch64-linux" ];
  #  config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
  # };
  # nix.settings.trusted-users = [ "@admin" ];
  # services.aerospace.settings.gaps.outer.top = lib.mkForce 42;
}
