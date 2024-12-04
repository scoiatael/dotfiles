{ config, lib, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./framework/hardware-configuration.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernelParams = [ "mem_sleep_default=deep" ];
  boot.kernel.sysctl."net.core.rmem_max" = 2500000;
  # Enroll keys once secureboot is enforced:
  # systemd-cryptenroll --tpm2-device=auto --tpm2-pcrs=7 /dev/nvme0n1p1
  boot.initrd.systemd = { enable = true; };

  security.sudo.enable = false;
  security.doas = { enable = true; };

  users.extraUsers.root.shell = pkgs.bash;
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;

  networking.hostName = "LsNixOS"; # Define your hostname.
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
    ];

    allowedUDPPortRanges = [ ];

    allowedUDPPorts = [ ];
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

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  #sound.enable = false;
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

  # https://wiki.archlinux.org/title/Solid_state_drive
  services.fstrim.enable = true;

  # Enable the Plasma 5 Desktop Environment.
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
  };
  services.desktopManager.plasma6.enable = true;
  # services.xserver.enable = true;
  # services.xserver.desktopManager.lxqt.enable = true;
  # disable the default
  # services.xserver.displayManager.lightdm.enable = false;
  # programs.sway = {
  #  enable = true;
  #   wrapperFeatures.gtk = true;
  # };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.l = {
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user.
      "networkmanager" # https://nixos.org/manual/nixos/stable/index.html#sec-networking
      "users"
      "i2c" # use ddcutil
      "samba-users"
    ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = (with pkgs; [
    pciutils
    lm_sensors
    yubioath-flutter
    git
    lsof
    go
    xsel
    xclip
    rclone
    kgpg
    gparted
    htop
    fzf
    ark
    sd
    zsh
    libsForQt5.kwallet
    libsForQt5.kate
    libsForQt5.krdc
    libsForQt5.bismuth
    libsForQt5.qt5ct
    libsForQt5.powerdevil
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
    aria2
  ]);

  fonts.packages = with pkgs; [
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
  nix.package = pkgs.lix; # or versioned attributes like nix_2_7
  environment.pathsToLink = [ "/share/nix-direnv" ];

  nix.settings.auto-optimise-store = true;
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 7d";
  };

  environment.sessionVariables = {
    MOZ_ENABLE_WAYLAND = "1";
    NIXOS_OZONE_WL = "1";
  };

  hardware.i2c.enable = true;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.settings = { General = { Experimental = true; }; };

  # https://nixos.wiki/wiki/Fwupd
  services.fwupd.enable = true;

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

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

}
