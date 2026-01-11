{ config, lib, pkgs, ... }:

{
  services.deluge = {
    enable = true;
    web.enable = true;
    declarative = true;
    authFile = "/etc/nixos/secrets/deluge-auth";
    config = {
      download_location = "/srv/nfs/quarantine/";
      max_upload_speed = "1000.0";
      share_ratio_limit = "0.5";
    };
  };
}
