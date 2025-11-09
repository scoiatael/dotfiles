{ config, lib, pkgs, ... }:

{
  # hardware.notch = true;

  homebrew = {
    taps = [{
      name = "scoiatael/dotfiles";
      clone_target = "https://github.com/scoiatael/dotfiles.git";
    }];
    casks = [
      "raindropio"
      "todoist-app"
      "discord"
      "steam"
      "bettertouchtool"
      "karabiner-elements"
      "balenaetcher"
      "keymapp"
      "proton-mail"
      "proton-pass"
      "arc"
      "secretive"
      "scoiatael/dotfiles/legimi-kindle"
      "astropad-studio"
    ];
    masApps = {
      "bitwarden" = 1352778147;
      "kagi" = 1622835804;
      "hush" = 1544743900;
      "Affinity Photo 2" = 1616822987;
      "DaisyDisk" = 411643860;
      "Proton Pass for Safari" = 6502835663;
    };
  };

  # NOTE: might require
  # sudo nix store ping --store ssh-ng://builder@linux-builder
  # to get it working
  nix.linux-builder = {
    enable = true;
    systems = [ "x86_64-linux" ];
    package = pkgs.darwin.linux-builder-x86_64;
  };
  nix.settings.sandbox = true;
}
