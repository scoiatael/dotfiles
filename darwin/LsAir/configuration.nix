{
  config,
  pkgs,
  lib,
  home-manager,
  nix-index-database,
  doomemacs,
  sops-nix,
  ...
}:

{
  imports = [
    sops-nix.darwinModules.sops
    home-manager.darwinModules.home-manager
    ../modules/default.nix
    ../modules/aerospace.nix
    ../modules/sketchybar.nix
    ../modules/lix.nix
    ../modules/openssh-host-keys.nix
  ];

  system.primaryUser = "lukaszczaplinski";
  ids.gids.nixbld = lib.mkForce 30000;
  networking.hostName = "LsAir";
  # hardware.notch = true;

  sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  sops.secrets.lukas_ssh_config = {
    format = "binary";
    sopsFile = ./secrets/ssh_config;
    owner = config.system.primaryUser;
    path = "${config.users.users.${config.system.primaryUser}.home}/.ssh/config";
  };

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
      "proton-pass"
      "arc"
      "secretive"
      "scoiatael/dotfiles/legimi-kindle"
      "astropad-studio"
      "tailscale-app"
      "eqmac"
      "petrichor"
    ];
    masApps = {
      "bitwarden" = 1352778147;
      "Affinity Photo 2" = 1616822987;
      "DaisyDisk" = 411643860;
      "Proton Pass for Safari" = 6502835663;
    };
  };

  nix.distributedBuilds = true;
  nix.settings.builders-use-substitutes = true;

  # nix.buildMachines = [
  #   {
  #     hostName = "192.168.180.153";
  #     sshUser = "remotebuild";
  #     sshKey = "/etc/nix/builder_ed25519";
  #     system = "x86_64-linux";
  #     supportedFeatures = [
  #       "nixos-test"
  #       "big-parallel"
  #       "kvm"
  #     ];
  #     protocol = "ssh-ng";
  #   }
  # ];

  nix.linux-builder = {
    enable = true;
    config.virtualisation.cores = 8;
    systems = [ "x86_64-linux" ];
    package = pkgs.darwin.linux-builder-x86_64;
  };
  nix.settings.sandbox = true;

  users.users.lukaszczaplinski = {
    name = "lukaszczaplinski";
    home = "/Users/lukaszczaplinski";
  };

  home-manager.users.lukaszczaplinski.imports = [
    ../../home/lukaszczaplinski-LsAir/configuration.nix
  ];
  home-manager.extraSpecialArgs = {
    inherit nix-index-database;
    inherit home-manager;
    inherit doomemacs;
  };
}
