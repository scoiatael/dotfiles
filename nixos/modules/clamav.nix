{ config, lib, pkgs, ... }:

{
  services.clamav = {
    updater.enable = true;
    daemon.enable = true;
    scanner = {
      enable = true;
      scanDirectories =
        [ "/home" "/var/lib" "/tmp" "/etc" "/var/tmp" "/srv/nfs/quarantine" ];
    };
  };
}
