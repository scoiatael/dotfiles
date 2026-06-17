{
  den,
  inputs,
  dotfiles,
  ...
}:
{
  den.hosts.x86_64-linux.tabletop-nixos.users.lukaszczaplinski = { };

  den.aspects.tabletop-nixos = {
    provides.lukaszczaplinski.includes = [
      den.aspects.neovim
      den.aspects.nnn
      den.aspects.zen
    ];
    provides.lukaszczaplinski.homeManager =
      {
        pkgs,
        lib,
        ...
      }:

      {
        imports = [
          dotfiles.homeModules.zsh
          dotfiles.homeModules.default
          dotfiles.homeModules.git
          dotfiles.homeModules.cli
          dotfiles.homeModules.linux
        ];

        programs.zsh.sessionVariables = {
          EDITOR = lib.getExe pkgs.neovim;
        };

        home = {
          username = "lukaszczaplinski";
          homeDirectory = "/home/lukaszczaplinski";
          stateVersion = "25.11";
        };
      };
    nixos =
      {
        pkgs,
        ...
      }:

      {
        imports = [
          ../_nixos/tabletop-nixos/hardware-configuration.nix
        ];

        # Bootloader.
        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        boot.initrd.luks.devices."luks-635483c0-a7da-41ea-815c-67af200fc196".device =
          "/dev/disk/by-uuid/635483c0-a7da-41ea-815c-67af200fc196";
        networking.hostName = "nixos"; # Define your hostname.
        # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

        # Configure network proxy if necessary
        # networking.proxy.default = "http://user:password@proxy:port/";
        # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

        # Enable networking
        networking.networkmanager.enable = true;

        # Set your time zone.
        time.timeZone = "Europe/Warsaw";

        # Select internationalisation properties.
        i18n.defaultLocale = "en_GB.UTF-8";

        i18n.extraLocaleSettings = {
          LC_ADDRESS = "pl_PL.UTF-8";
          LC_IDENTIFICATION = "pl_PL.UTF-8";
          LC_MEASUREMENT = "pl_PL.UTF-8";
          LC_MONETARY = "pl_PL.UTF-8";
          LC_NAME = "pl_PL.UTF-8";
          LC_NUMERIC = "pl_PL.UTF-8";
          LC_PAPER = "pl_PL.UTF-8";
          LC_TELEPHONE = "pl_PL.UTF-8";
          LC_TIME = "pl_PL.UTF-8";
        };

        # Enable the X11 windowing system.
        services.xserver.enable = true;

        # Enable the Budgie Desktop environment.
        services.xserver.desktopManager.budgie.enable = true;

        # Configure keymap in X11
        services.xserver.xkb = {
          layout = "pl";
          variant = "";
        };

        # Configure console keymap
        console.keyMap = "pl2";

        # Enable CUPS to print documents.
        services.printing.enable = true;

        # Enable sound with pipewire.
        services.pulseaudio.enable = false;
        security.rtkit.enable = true;
        services.pipewire = {
          enable = true;
          alsa.enable = true;
          alsa.support32Bit = true;
          pulse.enable = true;
          # If you want to use JACK applications, uncomment this
          #jack.enable = true;

          # use the example session manager (no others are packaged yet so this is enabled by default,
          # no need to redefine it in your config for now)
          #media-session.enable = true;
        };

        # Enable touchpad support (enabled default in most desktopManager).
        # services.xserver.libinput.enable = true;

        # Define a user account. Don't forget to set a password with ‘passwd’.
        users.users.lukaszczaplinski = {
          isNormalUser = true;
          description = "lukaszczaplinski";
          extraGroups = [
            "networkmanager"
            "wheel"
          ];
          openssh.authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJGdBIYMPRMhZ4KJ4excC5GjqvVKNFpxW0uUoH9zIqjQ"
            "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHJuPvZPJb9gGxF3EczaLOAUs4uzN06sk5AOSGGGozYy9j7xV+OdgTwVn3020l7Q85F0rCFBjRXyKm6uBOrilWw="
          ];
        };
        services.tailscale.enable = true;

        programs.zsh.enable = true;
        # Install firefox.
        programs.firefox.enable = true;

        # List packages installed in system profile. To search, run:
        # $ nix search wget
        environment.systemPackages = with pkgs; [
          #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
          #  wget
        ];

        # Some programs need SUID wrappers, can be configured further or are
        # started in user sessions.
        # programs.mtr.enable = true;
        # programs.gnupg.agent = {
        #   enable = true;
        #   enableSSHSupport = true;
        # };

        # List services that you want to enable:

        # Enable the OpenSSH daemon.
        services.openssh = {
          enable = true;
          # require public key authentication for better security
          settings.PasswordAuthentication = false;
          settings.KbdInteractiveAuthentication = false;
          settings.PermitRootLogin = "prohibit-password";
        };
        users.users.root.openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJGdBIYMPRMhZ4KJ4excC5GjqvVKNFpxW0uUoH9zIqjQ"
        ];

        # Open ports in the firewall.
        # networking.firewall.allowedTCPPorts = [ ... ];
        # networking.firewall.allowedUDPPorts = [ ... ];
        # Or disable the firewall altogether.
        # networking.firewall.enable = false;

        # This value determines the NixOS release from which the default
        # settings for stateful data, like file locations and database versions
        # on your system were taken. It‘s perfectly fine and recommended to leave
        # this value at the release version of the first install of this system.
        # Before changing this value read the documentation for this option
        # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
        system.stateVersion = "25.11"; # Did you read the comment?

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

        home-manager.extraSpecialArgs = inputs;
      };
  };
}
