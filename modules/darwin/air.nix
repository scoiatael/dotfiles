{ config, lib, pkgs, ... }:

{
  # hardware.notch = true;

  homebrew = {
    casks = [
      "todoist"
      "skype"
      "discord"
      "steam"
      "vlc"
      "keymapp"
      "battle-net"
      "${../../casks}/legimi-kindle"
      "bettertouchtool"
      "karabiner-elements"
      "arc"
"balenaetcher"
"keymapp"
    ];
    masApps = {
      "bitwarden" = 1352778147;
      "kagi" = 1622835804;
      "hush" = 1544743900;
      "grammarly" = 1462114288;
    };
  };
}
