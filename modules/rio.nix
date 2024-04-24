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
      shell = {
        program = lib.getExe pkgs.zsh;
        args =
          [ "-c" (lib.getExe pkgs.tmux) "new-session" "-A" "-s" "rio-main" ];
      };
      window = {
        background-opacity = 0.7;
        blur = true;
        decorations = "Disabled";
      };
      navigation = {
        mode = "Plain";
        clickable = true;
        use-current-path = true;
        color-automation = [ ];
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
