{ config, lib, pkgs, ... }:

{
  hardware.notch = true;

  homebrew = {
    casks = [ "todoist" "arc" "skype" ];
    masApps = {
      "bitwarden" = 1352778147;
      "kagi" = 1622835804;
    };
  };
}
