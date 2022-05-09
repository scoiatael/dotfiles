{ config, lib, pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "qt";
  };
  services.syncthing = {
    enable = true;
    tray = {
        enable = true;
      };
  };
  services.emacs = {
    enable = true;
  };
}
