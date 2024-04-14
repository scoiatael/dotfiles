{ pkgs, lib, ... }: {
  # TODO: https://github.com/gilacost/dot-files/blob/master/darwin-configuration.nix
  # TODO: https://the-empire.systems/linux-macos-setup
  # TODO: https://notes.alinpanaitiu.com/Keyboard%20tricks%20from%20a%20macOS%20app%20dev
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [ vim pinentry_mac ];

  # programs.fish.enable = true;
  programs.zsh = {
    enable = true; # default shell on catalina
    enableCompletion = false;
  };
  programs.gnupg = {
    agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  homebrew = {
    enable = true;
    casks = [
      "syncthing"
      "raycast"
      "signal"
      "lunar"
      "yubico-authenticator"
      "dteoh-devdocs"
      "caffeine"
    ];
    brews = [ "switchaudio-osx" ];
    taps = [ "Homebrew/homebrew-autoupdate" ];
    onActivation = { cleanup = "uninstall"; };
  };

  system.keyboard.enableKeyMapping = true;
  system.keyboard.remapCapsLockToControl =
    true; # Works alongside ControlEscape for better Caps :)

  system.defaults.dock.autohide = true;
  system.defaults.dock.mru-spaces = false;
  system.defaults.dock.orientation = "bottom";
  system.defaults.dock.show-recents = false;
  system.defaults.dock.static-only = true;

  system.defaults.finder.AppleShowAllExtensions = true;
  system.defaults.finder.QuitMenuItem = true;
  system.defaults.finder.FXEnableExtensionChangeWarning = false;
  system.defaults.finder.ShowStatusBar = true;

  system.defaults.spaces.spans-displays = false;

  system.defaults.".GlobalPreferences"."com.apple.mouse.scaling" = "-1.0";

  system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;
  system.defaults.NSGlobalDomain.NSDisableAutomaticTermination = true;
  system.defaults.NSGlobalDomain."com.apple.sound.beep.volume" = 0.0;
  system.defaults.NSGlobalDomain._HIHideMenuBar = true;

  # https://github.com/LnL7/nix-darwin/blob/master/modules/system/activation-scripts.nix#L111
  system.activationScripts.postUserActivation.text = let
    brewPrefix = if pkgs.stdenv.hostPlatform.isAarch64 then
      "/opt/homebrew/bin"
    else
      "/usr/local/bin";
  in ''
    echo >&2 "setting up homebrew autoupdate..."
    PATH=${brewPrefix}:$PATH brew autoupdate delete > /dev/null
    PATH=${brewPrefix}:$PATH brew autoupdate start --upgrade --cleanup

    # https://derflounder.wordpress.com/2023/09/26/managing-the-click-wallpaper-to-reveal-desktop-setting-in-macos-sonoma/
    /usr/bin/defaults write com.apple.WindowManager EnableStandardClickToShowDesktop -bool false
  '';
  # https://github.com/LnL7/nix-darwin/blob/master/modules/security/pam.nix#L25
  security.pam.enableSudoTouchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  documentation.doc.enable = false;

  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];
  nix.package = pkgs.nix;
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
