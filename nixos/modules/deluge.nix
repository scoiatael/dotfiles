{ config, lib, pkgs, ... }:

{
  services.deluge = {
    enable = true;
    web.enable = true;
    declarative = true;
    authFile = "/etc/nixos/secrets/deluge-auth";
    config = { download_location = "/srv/nfs/quarantine/"; };
  };
}
