{ config, lib, pkgs, grub2-themes, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    grub2-themes.nixosModules.default
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "nodev";
    efiSupport = true;
    enableCryptodisk = true;
    font = "${pkgs.hack-font}/share/fonts/hack/Hack-Regular.ttf";
    fontSize = 20;
  };
  boot.loader.grub2-theme = { theme = "whitesur"; };
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernelParams = [ "mem_sleep_default=deep" ];
  boot.kernel.sysctl."net.core.rmem_max" = 2500000;

  security.sudo.enable = false;
  security.doas = {
    enable = true;
    extraRules = [{
      users = [ "lczaplinski" ];
      runAs = "lczaplinski-docker";
      noPass = true;
      setEnv = [
        "AWS_ACCESS_KEY_ID"
        "AWS_DEFAULT_REGION"
        "AWS_REGION"
        "AWS_SECRET_ACCESS_KEY"
        "AWS_SECURITY_TOKEN"
        "AWS_SESSION_EXPIRATION"
        "AWS_SESSION_TOKEN"
        "AWS_VAULT"
      ];
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
    subUidRanges = [{
      startUid = 100000;
      count = 65536;
    }];
    subGidRanges = [{
      startGid = 100000;
      count = 65536;
    }];
  };

  users.extraUsers.root.shell = pkgs.bash;
  users.defaultUserShell = pkgs.zsh;

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

  # Open ports in the firewall.
  networking.firewall = {
    allowedTCPPorts = [
      6001 # shairport
      631 # avahi
    ];

    allowedUDPPortRanges = [{
      from = 6001;
      to = 6199;
    } # shairport
      ];

    allowedUDPPorts = [
      5353 # avahi
      631 # avahi
    ];
  };
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_GB.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };
  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-m32n.psf.gz";
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
    #TODO: https://github.com/coderonline/base16-vtrgb/blob/master/consolecolors/base16-nord.vga
    colors = [
      "002b36"
      "dc322f"
      "859900"
      "b58900"
      "268bd2"
      "d33682"
      "2aa198"
      "eee8d5"
      "002b36"
      "cb4b16"
      "586e75"
      "657b83"
      "839496"
      "6c71c4"
      "93a1a1"
      "fdf6e3"
    ];
  };

  # List services that you want to enable:

  # https://github.com/NixOS/nixpkgs/issues/126681
  services.interception-tools = {
    enable = true;
    plugins = [ pkgs.interception-tools-plugins.caps2esc ];
    udevmonConfig = ''
      - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc -m 1 | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
        DEVICE:
          EVENTS:
            EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
    '';
  };

  # https://nixos.wiki/wiki/Yubikey
  services.udev.packages = [ pkgs.yubikey-personalization ];

  services.pcscd.enable = true;

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
  };

  services.thermald.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = false;
  hardware.pulseaudio.enable = false;
  # https://nixos.wiki/wiki/PipeWire
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;
  };

  # https://nixos.wiki/wiki/Printing - for shairport
  services.avahi.enable = true;
  services.avahi.publish.enable = true;
  services.avahi.publish.userServices = true;

  # https://wiki.archlinux.org/title/Solid_state_drive
  services.fstrim.enable = true;

  # Enable the Plasma 5 Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;
  services.xserver.enable = true;
  # services.xserver.desktopManager.lxqt.enable = true;
  # disable the default
  # services.xserver.displayManager.lightdm.enable = false;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lczaplinski = {
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user.
      "networkmanager" # https://nixos.org/manual/nixos/stable/index.html#sec-networking
      "users"
      "i2c" # use ddcutil
    ];
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
  users.groups.lczaplinski =
    { }; # Create shared group between main user and -docker one
  system.activationScripts.shareHome = lib.stringAfter [ "users" ] ''
    chmod g+rwx /home/lczaplinski
  '';

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = (with pkgs; [
    git
    wget
    librewolf-wayland
    thunderbird-wayland
    # for testing
    ungoogled-chromium
    # ENDOF
    yakuake
    slack
    lsof
    fd
    zoom-us
    stow
    elvish
    zoxide
    go
    xsel
    xclip
    rclone
    signal-desktop
    kgpg
    gparted
    aws-vault
    google-cloud-sdk
    awscli
    aws-sam-cli
    htop
    fzf
    python3Minimal
    ark
    sd
    keybase-gui
    helix
    zsh
    libsForQt5.kwallet
    libsForQt5.kate
    libsForQt5.krdc
    libsForQt5.bismuth
    libsForQt5.qt5ct
    libsForQt5.powerdevil
    shairport-sync
    mpv
    shotcut
    powertop
    cpupower-gui
    # For waybar tray support
    libappindicator
    libappindicator-gtk3
    # modify external screen brightness
    brightnessctl
    ddcutil
    ddcui
    # soundcontrol
    pavucontrol
    # gsettings
    glib
    # Sway nice-to-haves
    wl-clipboard
    nm-tray
    adwaita-qt
    # Screenshotting under sway
    sway-contrib.grimshot
  ]);

  # SLACK!
  nixpkgs.config.allowUnfree = true;

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "FiraCode" "DroidSansMono" ]; })
    cozette
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  # programs.ssh.startAgent = false;

  # https://github.com/nix-community/nix-direnv#via-configurationnix-in-nixos
  # at least until I have home-manager working properly :)
  # nix options for derivations to persist garbage collection
  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
    experimental-features = nix-command flakes
  '';
  # https://nixos.wiki/wiki/Flakes
  nix.package = pkgs.nixFlakes; # or versioned attributes like nix_2_7
  environment.pathsToLink = [ "/share/nix-direnv" ];

  nix.settings.auto-optimise-store = true;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  # https://nixos.wiki/wiki/Accelerated_Video_Playback
  nixpkgs.config.packageOverrides = pkgs: {
    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
  };
  hardware.opengl = {
    enable = true;
    driSupport = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  environment.sessionVariables.NIXOS_OZONE_WL = "1";

  environment.sessionVariables = { MOZ_ENABLE_WAYLAND = "1"; };

  hardware.i2c.enable = true;

  hardware.bluetooth.enable = true;

  environment.etc."NetworkManager/dispatcher.d/99-wlan" = {
    text = ''
      #!${pkgs.bash}/bin/bash
      wired_interfaces="en.*|eth.*"
      if [[ "$1" =~ $wired_interfaces ]]; then
          case "$2" in
              up)
                  nmcli radio wifi off
                  ;;
              down)
                  nmcli radio wifi on
                  ;;
          esac
      fi
    '';

    mode = "0550";
  };

  environment.etc."pam.d/gtklock" = {
    text = "auth include login";
    mode = "0550";
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
