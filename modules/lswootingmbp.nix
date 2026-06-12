{
  inputs,
  ...
}:
{
  den.hosts.aarch64-darwin.lswootingmbp.users.lukas = { };

  den.aspects.lswootingmbp = {
    provides.lukas.homeManager =
      { pkgs, lib, ... }:

      {
        imports = [
          ../home/modules/default.nix
          ../home/modules/cli.nix
          ../home/modules/home-manager.nix
          ../home/modules/git.nix
          ../home/modules/multiplexers/tmux.nix
          ../home/modules/shells/zsh.nix
          ../home/modules/editors/emacs.nix
          ../home/modules/terminals/wezterm.nix
          ../home/modules/llm.nix
          ../home/modules/editors/neovim.nix
          ../home/modules/comma.nix
          ../home/modules/graphite.nix
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

    darwin =
      {
        pkgs,
        sops-nix,
        ...
      }:

      {
        imports = [
          sops-nix.darwinModules.sops
          ../darwin/modules/default.nix
          ../darwin/modules/aerospace.nix
          ../darwin/modules/sketchybar.nix
          ../darwin/modules/lix.nix
          ../darwin/modules/openssh-host-keys.nix
          ../darwin/modules/compose-key/default.nix
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
            "thebrowsercompany-dia"
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
