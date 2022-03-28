# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
      enable = true;
      version = 2;
      device = "nodev";
      efiSupport = true;
      enableCryptodisk = true;
  };
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [ "mem_sleep_default=deep" ];

  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [{
	 users = [ "lczaplinski" ];
         runAs = "lczaplinski-docker";
         noPass = true;
         setEnv = [  "AWS_ACCESS_KEY_ID" "AWS_DEFAULT_REGION" "AWS_REGION" "AWS_SECRET_ACCESS_KEY" "AWS_SECURITY_TOKEN" "AWS_SESSION_EXPIRATION" "AWS_SESSION_TOKEN" "AWS_VAULT" ];
    }];
  };

  # https://rdes.gitlab.io/posts/2016-08-29-enabling-dockers-user-namespaces-in-nixos.html
  virtualisation.docker = {
    enable = true;
    extraOptions = "--userns-remap=default";
  };
  users.groups.dockremap.gid = 10000;
  users.users.dockremap = {
    isSystemUser = true;
    uid = 10000;
    group = "dockremap";
    subUidRanges = [
      { startUid = 100000; count = 65536; }
    ];
    subGidRanges = [
      { startGid = 100000; count = 65536; }
    ];
  };

  networking.hostName = "r-work-nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.wireless.userControlled.enable = true;
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # networking.useDHCP = false;
  # networking.interfaces.enp0s13f0u1u4.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;


  # Enable the Plasma 5 Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  

  # Configure keymap in X11
  services.xserver.layout = "pl";
  services.xserver.xkbOptions = "caps:escape";

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  services.xserver.libinput.enable = true;

  # https://nixos.wiki/wiki/Printing - for shairport
  services.avahi.enable = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lczaplinski = {
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user.
      "networkmanager" # https://nixos.org/manual/nixos/stable/index.html#sec-networking
      "users"
    ];
    shell = pkgs.elvish;
    group = "lczaplinski";
  };
  users.users.lczaplinski-docker = {
    isSystemUser = true;
    group = "lczaplinski";
    extraGroups = [
      "docker" # Enable ‘docker’ for the user.
    ];
    home = "/home/docker";
    createHome = true;
  };
  users.groups.lczaplinski = {}; # Create shared group between main user and -docker one
  system.activationScripts.shareHome = lib.stringAfter [ "users" ] ''
    chmod g+rwx /home/lczaplinski
  '';

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    firefox
    emacsGcc
    yakuake
    yubioath-desktop
    git
    slack
    ripgrep
    fd
    tmux
    zoom-us
    stow
    shairport-sync
    elvish
    zoxide
    exa
    go
    xsel
    xclip
    rclone
    signal-desktop
    kgpg
    bat
    gparted
    aws-vault
    google-cloud-sdk
    awscli
    aws-sam-cli
    htop
    fzf
    python3Minimal
    vscode
    ark
    direnv nix-direnv 
    sd
    keybase-gui
    wally-cli
  ];

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
  ];

  # SLACK!
  nixpkgs.config.allowUnfree = true; 

  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  programs.ssh.startAgent = false;

  # https://github.com/nix-community/nix-direnv#via-configurationnix-in-nixos
  # at least until I have home-manager working properly :)
  # nix options for derivations to persist garbage collection
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';
  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  # List services that you want to enable:

  # https://nixos.wiki/wiki/Yubikey
  services.udev.packages = [ pkgs.yubikey-personalization ];

  services.pcscd.enable = true;

  services.keybase.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  networking.firewall = {
    allowedTCPPorts = [ 6001 ];

    allowedUDPPortRanges = [
      { from = 6001; to = 6199; } # shairport
    ];

    allowedUDPPorts = [
      5353 # avahi
    ];
  };
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}

