{ config, lib, pkgs, ... }:

let
  ghosttyDesktop = pkgs.stdenvNoCC.mkDerivation rec {
    name = "ghostty-launcher";
    dontBuild = true;

    unpackPhase = "true";

    desktopItem = pkgs.makeDesktopItem {
      name = "ghostty-launcher";
      exec = lib.getExe pkgs.ghostty;
      genericName = "GhosTTY";
      categories = [ "System" "TerminalEmulator" ];
      icon = "terminal";
      desktopName = "Ghostty";
    };

    installPhase = ''
      mkdir -p $out/share
      cp -r ${desktopItem}/share/applications $out/share
    '';

  };
in {
  home.packages = [ pkgs.ghostty ghosttyDesktop ];

  home.file.".config/ghostty/config".text = ''
    theme = catppuccin-frappe
    font-size = 9
  '';
}
