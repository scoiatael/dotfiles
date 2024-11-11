{ lib, pkgs, ... }:

{
  # hardware.notch = false;
  homebrew = {
    casks = [ "slack" "betterdisplay" "gather" "bitwarden" "arc" "wezterm" ];
  };
  nix.linux-builder.enable = true;
  nix.settings.extra-trusted-users = [ "@admin" ];
  services.aerospace.settings.gaps.outer.top = lib.mkForce 42;
}
