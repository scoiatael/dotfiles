{ inputs, ... }:

{
  flake-file.inputs = {
    niri-nix.url = "git+https://codeberg.org/BANanaD3V/niri-nix";
    noctalia.url = "github:noctalia-dev/noctalia";
    pixie-sddm.url = "github:xCaptaiN09/pixie-sddm";
  };
  den.aspects.nnn = {
    nixos =
      { pkgs, lib, ... }:
      {
        imports = [ inputs.niri-nix.nixosModules.default ];
        services.displayManager.sddm = {
          theme = "pixie";
          package = lib.mkForce pkgs.kdePackages.sddm;
          extraPackages = [
            pkgs.kdePackages.qtsvg
            pkgs.kdePackages.qtdeclarative
            pkgs.kdePackages.qt5compat
          ];

          # Fix for NixOS explicitly requiring a cursor theme
          settings = {
            Theme = {
              CursorTheme = "breeze_cursors"; # Change this if you use a different cursor theme (e.g., Adwaita)
            };
          };
        };
        environment.systemPackages =
          let
            inherit (inputs.pixie-sddm.packages.${pkgs.stdenv.hostPlatform.system}) pixie-sddm;
            noctalia = inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default;
          in
          [
            pixie-sddm
            noctalia
            pkgs.xwayland-satellite
          ];
        hardware.bluetooth.enable = true;
        services.power-profiles-daemon.enable = true;
        services.upower.enable = true;
        programs.niri = {
          enable = true;
          # package = pkgs.niri-unstable;
        };
      };
    homeManager = {
      imports = [
        inputs.niri-nix.homeModules.default
        inputs.niri-nix.homeModules.stylix
        inputs.noctalia.homeModules.default
      ];
      wayland.windowManager.niri.enable = true;
      wayland.windowManager.niri.settings = {
        spawn-at-startup = "noctalia";

        debug = {
          #/ Allows notification actions and window activation from Noctalia.
          honor-xdg-activation-with-invalid-serial = true;
        };

        binds = import _nnn/niri-keybinds.nix;
        window-rule = import _nnn/niri-window-rule.nix;

        # Screenshot
        screenshot-path = "~/Pictures/Screenshots/screenshot-%Y-%m-%d-%H-%M-%S.png";

        # Displays
        output = [
          {
            _args = [ "HDMI-A-1" ];
            mode = "3840x2160@60.000";
            scale = 2;
          }
        ];
      };
      programs.noctalia = {
        enable = true;

        settings = {
          theme = {
            mode = "dark";
            source = "builtin";
            builtin = "Catppuccin";
          };
        };
      };
    };
  };
}
