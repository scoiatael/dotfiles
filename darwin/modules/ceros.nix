{ lib, pkgs, ... }:

{
  hardware.notch = true;
  homebrew = {
    casks = [
      "slack"
      "zoom"
      "google-drive"
      "1password"
      "yubico-yubikey-manager"
      "arc"
      "docker"
      "1password/tap/1password-cli"
      "routine"
    ];
  };
}
