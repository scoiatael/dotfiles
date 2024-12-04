{ config, lib, pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryPackage = pkgs.pinentry-gnome3;
  };
  programs.zsh = {
    sessionVariables = {
      # nixOS workaround for https://github.com/99designs/aws-vault/issues/670
      AWS_VAULT_BACKEND = "kwallet";
    };
  };

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
    bitwarden
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
  programs.wezterm.enable = true;
}
