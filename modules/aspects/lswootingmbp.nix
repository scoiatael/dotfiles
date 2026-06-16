{
  den,
  inputs,
  dotfiles,
  ...
}:
{
  den.hosts.aarch64-darwin.LsWootingMBP.users.lukas = { };

  den.aspects.LsWootingMBP = {
    provides.lukas = {
      includes = [
        den.aspects.llm
        den.aspects.doomemacs
        den.aspects.comma
        den.aspects.graphite
        den.aspects.neovim
        den.aspects.stylix
        den.aspects.zen
        (den.batteries.user-shell "zsh")
        den.batteries.mkBackupCommand
      ];
      homeManager =
        { pkgs, lib, ... }:

        {
          imports = [
            dotfiles.homeModules.zsh
            ../_home/modules/default.nix
            ../_home/modules/cli.nix
            ../_home/modules/git.nix
            ../_home/modules/multiplexers/tmux.nix
            ../_home/modules/terminals/wezterm.nix
          ];

          accounts.email.accounts = {
            "lukasz@wooting.io" = {
              primary = true;
              himalaya.enable = true;
              imap = {
                port = 993;
                host = "imap.gmail.com";
              };
              realName = "Lukas Czaplinski";
              userName = "lukasz@wooting.io";
              passwordCommand = [
                (lib.getExe pkgs.pass)
                "himalaya-gmail-app-password"
              ];
              address = "lukasz@wooting.io";
            };
          };

          programs.himalaya.enable = true;

          programs.git.settings.user = {
            email = "lukasz@wooting.io";
            name = "Lukas Czaplinski";
            signingkey = "E871295C0EFA7DBFA9E673CC7135745D2C62273D";
          };

          home = {
            username = "lukas";
            homeDirectory = "/Users/lukas";
            stateVersion = "22.05";
          };
        };
    };

    darwin =
      {
        pkgs,
        ...
      }:

      {
        imports = [
          inputs.sops-nix.darwinModules.sops
          ../_darwin/modules/default.nix
          ../_darwin/modules/aerospace.nix
          ../_darwin/modules/sketchybar.nix
          ../_darwin/modules/lix.nix
          ../_darwin/modules/openssh-host-keys.nix
          ../_darwin/modules/compose-key/default.nix
        ];

        system.primaryUser = "lukas";

        users.users.lukas = {
          name = "lukas";
          home = "/Users/lukas";
        };

        # hardware.notch = false;
        homebrew = {
          casks = [
            "bitwarden"
            "slack"
            "google-drive"
            "monodraw"
            "altair-graphql-client"
            "forklift"
            "mitmproxy"
            "astropad-studio"
            "utm"
            "notion"
            "tailscale-app"
            "eqmac"
            "keymapp"
          ];
          brews = [ "stlink" ];
        };
        nix.linux-builder = {
          enable = true;
          systems = [ "x86_64-linux" ];
          package = pkgs.darwin.linux-builder-x86_64;
        };
        #  nix.linux-builder = {
        #   enable = true;
        #  systems = [ "x86_64-linux" "aarch64-linux" ];
        #  config.boot.binfmt.emulatedSystems = [ "x86_64-linux" ];
        # };
        # nix.settings.trusted-users = [ "@admin" ];
        # services.aerospace.settings.gaps.outer.top = lib.mkForce 42;
        home-manager.extraSpecialArgs = inputs;
      };
  };
}
