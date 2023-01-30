{ config, lib, pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "qt";
  };
  services.keybase.enable = true;
  services.syncthing = {
    enable = true;
    # tray = {
    #     enable = true;
    #     package = pkgs.qsyncthingtray;
    #     command = "qsyncthingtray --wait";
    #   }; doesn't work due to missing 'tray.target'
  };
  services.emacs.enable = true;
  home.packages = with pkgs; [
    janet # broken on macOS
    procs # broken on macOS
    libnotify # doesn't work on macOS
    lm_sensors # for temp display in i3status-rust?
    neofetch
  ];
  services.unclutter.enable = true;
  xresources.properties = {
    "Xft.antialias" = true;
    "Xft.autohint" = false;
    "Xft.rgba" = "rgb";
    "Xft.hinting" = true;
    "Xft.hintstyle" = "hintfull";
    "Xft.lcdfilter" = "lcddefault";
  };
}
