{ config, lib, pkgs, ... }:

{
  hardware.notch = true;

  homebrew = {
    casks = [
      "bitwarden"
      "todoist"
      "arc"
    ];
  };
}
