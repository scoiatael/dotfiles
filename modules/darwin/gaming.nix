{ config, lib, pkgs, ... }:

{
  homebrew = {
    casks = [
      "battle-net"
      "bitwarden"
      "eloston-chromium"
      "todoist"
      "dropbox"
      "wowup"
      "steam"
      "skype"
      "discord"
    ];
  };
}
