{ config, lib, pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "qt";
  };
  programs.zsh.sessionVariables = {
    # nixOS workaround for https://github.com/99designs/aws-vault/issues/670
    AWS_VAULT_BACKEND = "kwallet";
    # error: file 'nixpkgs' was not found in the Nix search path (add it using $NIX_PATH or -I)
    NIX_PATH = "~/.nix-defexpr/channels/nixos";
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
