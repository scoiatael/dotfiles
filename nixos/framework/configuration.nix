{ config, lib, pkgs, ... }:

{
  imports = [
    ../modules/smb.nix
    ../modules/blocky.nix
    ../modules/jellyfin.nix
    ../modules/steam.nix
    ../modules/scrutiny.nix
    ../modules/yarr.nix
    ../modules/restic.nix
    ../modules/tailscale.nix
    ../modules/telegraf.nix
    ../modules/parrhasius.nix
    ../modules/mail.nix
    ../modules/grafana.nix
    ../modules/remote-builder.nix
    ../modules/deluge.nix
    ../modules/clamav.nix
    ./tailscale-services.nix
    ./caddy.nix
    ./hardware-configuration.nix
  ];
  programs.mosh.enable = true;

  users.users.remotebuild.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIESt/O98FeSfgnLBfit9jE0hgNP3Ww0wpP+r1QGjaeNc builder@localhost" # builder key on LsAir
  ];

  services.syncthing = {
    enable = true;
    user = "lukaszczaplinski";
    overrideDevices = false;
    overrideFolders = false;
    dataDir = "/home/lukaszczaplinski";
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = false;
  boot.loader.efi.efiSysMountPoint = "/boot/EFI";
  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.kernelParams = [ "mem_sleep_default=deep" "usbcore.quirks=0bda:8156:k" ];
  boot.kernel.sysctl."net.core.rmem_max" = 2500000;
  # Enroll keys once secureboot is enforced:
  # systemd-cryptenroll --tpm2-device=auto --tpm2-pcrs=7 /dev/nvme0n1p1
  boot.initrd.systemd = { enable = true; };

  hardware.graphics.extraPackages = with pkgs; [
    intel-compute-runtime
    intel-media-driver
  ];

  security.sudo.enable = false;
  security.doas = { enable = true; };

  users.extraUsers.root.shell = pkgs.bash;
  users.defaultUserShell = pkgs.zsh;
  programs.zsh.enable = true;

  networking.hostName = "LsFramework"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.wireless.userControlled.enable = true;
  # networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Warsaw";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  # networking.useDHCP = false;
  # networking.interfaces.enp0s13f0u1u4.useDHCP = true;
  networking = {
    interfaces.enp0s13f0u1 = {
      ipv4.addresses = [{
        address = "192.168.1.153";
        prefixLength = 24;
      }];
    };
    defaultGateway = {
      address = "192.168.1.1";
      interface = "enp0s13f0u1";
    };
    nameservers = [ "127.0.0.1" ];
  };

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Open ports in the firewall.
  networking.firewall = {
    allowedTCPPorts = [
      22000 # syncthing
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
  services.openssh = {
    enable = true;
    # require public key authentication for better security
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
    settings.PermitRootLogin = "prohibit-password";
  };

  services.clamav = {
    daemon.enable = true;
    updater.enable = true;
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  #sound.enable = false;
  services.pulseaudio.enable = false;
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
  # https://blog.stigok.com/2019/12/09/nixos-avahi-publish-service.html
  services.avahi = {
    enable = true;
    publish = {
      enable = true;
      addresses = true;
      workstation = true;
      userServices = true;
    };
    allowInterfaces = [ "enp4s0" ];
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
  users.users.lukaszczaplinski = {
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable ‘sudo’ for the user.
      "networkmanager" # https://nixos.org/manual/nixos/stable/index.html#sec-networking
      "users"
      "i2c" # use ddcutil
      "samba-users"
    ];
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC1hKsM+Xk4QAzHUI3p4stYXvLlUF6lyHn9UzkduK44A7ctN53GRtqu308cRpqOb4rIH9Ohu3ZuyvofmwxfIuKfOnTbVlnhx6w/Dbm7P+9TAq7ZjjSiHmBp+1tn81t0sdCwVSa20jFTztHB0eJ0SnZaKkBkSu8+Y8Ul66Of8R8ehtxsVLGeleOIg7peBIy2YU7Flmz0Tg3ZEsA5jNgwTkeKYKcMNIMjhDM1l5mqvsj76mPtulpJxdW2YeN4td+OW2d918bWgf245/nqyWjFVEr9ftc4MFSit16P2emm/IKxo8H7tb2Pb29JrPtjuCdPVcrj5u/wyVBAx7Hgo9l8YZsLxwN5+CelF67rHzR1yXfmVqeN+sBJC/ZrjOgubl/b/bwJWw04eZbDwoCDuC1DdLTT8SH2/QZRsIf4ay3+bLNc+RD3VsB3QPifUWMYiTJxWJ81tO/qU+IBnlN/WW0kUBLk2Md3ybJFvGh4NoSu9Gi3PguGNpth0NG3a7pgK9al5wP98/3ANidlH/r+XU5bB0ewQ64gDN0w7lpwmI2yemfnVmPxlzGODm6Vr7UbX9rwNAGHQf0oXSWd1uIL3eOfqMVXLGmMwcuh423VijajXRp32VtlAPQo7NsTz3ankf0SxBIhIsy3/kuvbvbfyIxVVoXypZnrezRaLqtgNCT6j+/b8Q== keybase"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ7QCZSnf0WDnN8EnI6h8la4xIMeetYhTh4y70YouZUF bitwarden"
      "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHJuPvZPJb9gGxF3EczaLOAUs4uzN06sk5AOSGGGozYy9j7xV+OdgTwVn3020l7Q85F0rCFBjRXyKm6uBOrilWw= SSH-with-auth@secretive.LsAir.local"
    ];
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHqPfAOEkybKC4aBtpysiR0zqLJGqZfL2JIMhQc5HTR0AOt6MWT3u7RgxqguoZGeLofrA0Egbf55+KSy9+jE/8E= air@scoiatael.dev"
  ];

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
    signal-desktop
    keybase-gui
    gparted
    htop
    fzf
    sd
    zsh
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
    adwaita-qt
    # Screenshotting under sway
    sway-contrib.grimshot
    aria2
  ]);

  fonts.packages = with pkgs; [
    nerd-fonts.fira-code
    nerd-fonts.droid-sans-mono
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
