{ config, lib, pkgs, ... }:

{
  hardware.notch = true;

  homebrew = {
    casks = [ "todoist" "arc" "skype" "canon-eos-utility" "discord" "steam" "vlc" ];
    masApps = {
      "bitwarden" = 1352778147;
      "kagi" = 1622835804;
      "hush" = 1544743900;
    };
  };
}
