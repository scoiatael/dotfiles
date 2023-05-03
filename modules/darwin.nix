{ pkgs, lib, ... }: {
  # TODO: https://github.com/gilacost/dot-files/blob/master/darwin-configuration.nix
  # TODO: https://the-empire.systems/linux-macos-setup
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
    pinentry_mac
    (callPackage ../packages/sketchybar-helper { })
  ];

  # programs.fish.enable = true;
  programs.zsh.enable = true; # default shell on catalina
  programs.gnupg = {
    agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  homebrew = {
    enable = true;
    casks = [
      "karabiner-elements"
      "syncthing"
      "keybase"
      "raycast"
      "librewolf"
      "signal"
      "lunar"
    ];
    taps = [ "Homebrew/homebrew-autoupdate" ];
    onActivation = { cleanup = "uninstall"; };
  };

  # https://github.com/LnL7/nix-darwin/blob/master/modules/system/activation-scripts.nix#L111
  system.activationScripts.postUserActivation.text = let
    brewPrefix = if pkgs.stdenv.hostPlatform.isAarch64 then
      "/opt/homebrew/bin"
    else
      "/usr/local/bin";
  in ''
    echo >&2 "setting up homebrew autoupdate..."
    PATH=${brewPrefix}:$PATH brew autoupdate delete
    PATH=${brewPrefix}:$PATH brew autoupdate start --upgrade --cleanup
  '';
  # https://github.com/LnL7/nix-darwin/blob/master/modules/security/pam.nix#L25
  security.pam.enableSudoTouchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  nix.package = pkgs.nix;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
