{ config, lib, pkgs, ... }:

{
  services.caddy.enable = true;
  services.caddy.virtualHosts = {
    "parrhasius.heron-pollux.ts.net".extraConfig = ''
      reverse_proxy :4567
    '';
    "yarr.heron-pollux.ts.net".extraConfig = ''
      reverse_proxy :7070
    '';
    "scrutiny.heron-pollux.ts.net".extraConfig = ''
      reverse_proxy :8053
    '';
    "jellyfin.heron-pollux.ts.net".extraConfig = ''
      reverse_proxy :8096
    '';
    "influxdb.heron-pollux.ts.net".extraConfig = ''
      reverse_proxy :8086
    '';
  };

  services.tailscale.permitCertUid = config.services.caddy.user;

  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
