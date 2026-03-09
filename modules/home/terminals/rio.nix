{ config, lib, catppuccin-rio, rio, pkgs, ... }:

{
  programs.rio = {
    package = pkgs.rio.overrideAttrs (drv: rec {
      version = "0.0.x";
      src = rio;
      cargoDeps = drv.cargoDeps.overrideAttrs (_: {
        name = "${drv.pname}-${version}-vendor.tgz";
        inherit src;
        outputHash = "sha256-wQ0PbmY3klLu2hfADYZaUj6wn5q0RNcdLGihNQzlMKY=";
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
        # mode = "Breadcrumb";
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
            key = "pageup";
            action = "ScrollHalfPageUp";
          }
          {
            key = "pagedown";
            action = "ScrollHalfPageDown";
          }
          {
            key = "up";
            "with" = "super";
            action = "ScrollHalfPageUp";
          }
          {
            key = "down";
            "with" = "super";
            action = "ScrollHalfPageDown";
          }
        ];
      };
      fonts = {
        family = "JetBrainsMono Nerd Font";
        size = 16;
      };
      env-vars = [ ];
      theme = "catppuccin-frappe";
    };
  };

  home.file.".config/rio/themes" = {
    source = catppuccin-rio;
    recursive = true;
  };
}
