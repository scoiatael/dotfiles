{ config, lib, pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "qt";
  };
  services.syncthing = {
    enable = true;
    # tray = {
    #     enable = true;
    #     package = pkgs.qsyncthingtray;
    #     command = "qsyncthingtray --wait";
    #   }; doesn't work due to missing 'tray.target'
  };
  services.emacs = {
    enable = true;
  };
  home.packages = with pkgs; [
    janet # broken on macOS
    procs # broken on macOS
    libnotify # doesn't work on macOS
  ];
  programs.qutebrowser.enable = true;
}
