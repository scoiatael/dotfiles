{ lib, pkgs, ... }:

{
  hardware.notch = false;
  homebrew = {
    casks = [
      "slack"
"betterdisplay"
"gather"
"bitwarden"
    ];
  };
}
