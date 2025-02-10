{ lib, pkgs, ... }:

{
  # hardware.notch = false;
  homebrew = {
    casks = [ "bitwarden" "arc" "wezterm" "slack" ];
    brews = [ "stlink" ];
  };
  # nix.linux-builder.enable = true;
  nix.settings.extra-trusted-users = [ "@admin" ];
  # services.aerospace.settings.gaps.outer.top = lib.mkForce 42;
}
