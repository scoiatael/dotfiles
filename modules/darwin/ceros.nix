{ lib, pkgs, ... }:

{
  hardware.notch = true;
  homebrew = {
    casks = [
      "loom"
      "slack"
      "zoom"
      "google-drive"
      "1password"
      "yubico-yubikey-manager"
      "arc"
      "notion"
      "keymapp"
      "iterm2"
      "gitbutler"
    ];
  };
}
