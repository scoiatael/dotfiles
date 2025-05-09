{ config, lib, pkgs, ... }:

{
  # hardware.notch = true;

  homebrew = {
    casks = [
      "todoist"
      "discord"
      "steam"
      "${../../casks}/legimi-kindle"
      "bettertouchtool"
      "karabiner-elements"
      "balenaetcher"
      "keymapp"
      "proton-mail"
      "proton-pass"
      "arc"
    ];
    masApps = {
      "bitwarden" = 1352778147;
      "kagi" = 1622835804;
      "hush" = 1544743900;
      "Affinity Photo 2" = 1616822987;
      "DaisyDisk" = 411643860;
      "Proton Pass for Safari" = 6502835663;
      "SnippetsLab" = 1006087419;
    };
  };
}
