{ lib, pkgs, ... }:

{
  hardware.notch = true;
  homebrew = {
    casks =
      [ "slack" "zoom" "google-drive" "1password" "yubico-yubikey-manager" ];
  };
}
