{ config, lib, pkgs, ... }:

{
  programs.rio = {
    package = pkgs.rio.overrideAttrs (drv: rec {
      version = "main-02e6c3075a7ed946caf2a9e5c0ebd6903e7afc74";
      src = pkgs.fetchFromGitHub {
        owner = "raphamorim";
        repo = "rio";
        rev = "02e6c3075a7ed946caf2a9e5c0ebd6903e7afc74";
        hash = "sha256-wg2awdzgOXkXGuURn4AuEco2OJyTqeRDaDrds0TR7k4=";
      };
      cargoDeps = drv.cargoDeps.overrideAttrs (_: {
        name = "${drv.pname}-${version}-vendor.tgz";
        inherit src;
        outputHash = "sha256-i2IhrsopzDDJaNy+E+VrDb6rbenrDOSEvQx8EzAnUbE=";
      });
      buildInputs = with pkgs; [
        darwin.libobjc
        darwin.apple_sdk_11_0.frameworks.AppKit
        darwin.apple_sdk_11_0.frameworks.AVFoundation
        darwin.apple_sdk_11_0.frameworks.Vision
        darwin.apple_sdk_11_0.frameworks.MetalKit
        iconv
      ];
    });
    enable = true;
    settings = {
      "confirm-before-quit" = false;
      "hide-cursor-when-typing" = true;
      # shell = {
      #   program = lib.getExe pkgs.zsh;
      #   args = [ "-c" "${(lib.getExe pkgs.tmux)} new-session -A -s rio-main" ];
      # };
      window = {
        background-opacity = 0.7;
        blur = true;
      };
      navigation = {
        mode = "CollapsedTab";
        clickable = false;
        use-current-path = true;
        color-automation = [
          {
            program = "nvim";
            color = "#FFFF00";
          }
          {
            path = "~/dotfiles";
            color = "#FF0000";
          }
          {
            path = "gemma/modules/ui";
            color = "#FFaabb";
          }
          {
            path = "gemma/modules/api";
            color = "#FFbbaa";
          }
          {
            path = "gemma/infrastructure";
            color = "#FFbbbb";
          }
        ];
      };
      bindings = {
        keys = [
          {
            key = "t";
            "with" = "super";
            action = "CreateTab";
          }
          {
            key = "n";
            "with" = "super";
            action = "CreateWindow";
          }
          {
            key = "w";
            "with" = "super";
            action = "CloseTab";
          }
          {
            key = "q";
            "with" = "super";
            action = "Quit";
          }
          {
            key = "pageup";
            action = "ScrollPageUp";
          }
          {
            key = "pagedown";
            action = "ScrollPageDown";
          }
          {
            key = "]";
            "with" = "super";
            action = "SelectNextTab";
          }
          {
            key = "[";
            "with" = "super";
            action = "SelectPrevTab";
          }
          {
            key = "c";
            "with" = "super";
            action = "Copy";
          }
          {
            key = "v";
            "with" = "super";
            action = "Paste";
          }
        ];
      };
      fonts = {
        family = "JetBrainsMono Nerd Font";
        size = 16;
      };
      env-vars = [ ];
      theme = "catpuccin-frappe";
    };
  };

  home.file.".config/rio/themes" = {
    source = ./rio/themes;
    recursive = true;
  };
}
