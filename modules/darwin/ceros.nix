{ lib, pkgs, ... }:

{
  hardware.notch = true;
  homebrew = { casks = [ "slack" "zoom" "google-drive" ]; };
}
