{ config, lib, pkgs, ... }:

{
  # hardware.notch = true;

  homebrew = {
    taps = [
      {
        name = "scoiatael/dotfiles";
        clone_target = "https://github.com/scoiatael/dotfiles.git";
      }
      "RhetTbull/osxphotos"
    ];
    brews = [ "rhettbull/osxphotos/osxphotos" ];
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

  nix.distributedBuilds = true;
  nix.settings.builders-use-substitutes = true;

  nix.buildMachines = [{
    hostName = "192.168.1.153";
    sshUser = "remotebuild";
    sshKey = "/root/.ssh/remotebuild";
    system = "x86_64-linux";
    supportedFeatures = [ "nixos-test" "big-parallel" "kvm" ];
  }];

  # nix.linux-builder = {
  #   enable = true;
  #   config.virtualisation.cores = 8;
  # };
  nix.settings.sandbox = true;
}
