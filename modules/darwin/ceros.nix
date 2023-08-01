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
    ];

    brews = [{
      name = "d12frosted/emacs-plus/emacs-plus@29";
      link = true;
    }];

    taps = [ "d12frosted/emacs-plus" ];
  };
}
