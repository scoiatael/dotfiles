{
  config,
  lib,
  pkgs,
  ...
}:

{
  services.caddy.enable = true;
  services.caddy.virtualHosts."parrhasius.heron-pollux.ts.net".extraConfig = ''
    reverse_proxy :4567
  '';
  services.caddy.virtualHosts."yarr.heron-pollux.ts.net".extraConfig = ''
    reverse_proxy :7070
  '';
  services.caddy.virtualHosts."scrutiny.heron-pollux.ts.net".extraConfig = ''
    reverse_proxy :8053
  '';
  services.caddy.virtualHosts."jellyfin.heron-pollux.ts.net".extraConfig = ''
    reverse_proxy :8096
  '';
  services.caddy.virtualHosts."influxdb.heron-pollux.ts.net".extraConfig = ''
    reverse_proxy :8086
  '';
  services.caddy.virtualHosts."grafana.heron-pollux.ts.net".extraConfig = ''
    reverse_proxy :3000
  '';
  services.caddy.virtualHosts."deluge.heron-pollux.ts.net".extraConfig = ''
    reverse_proxy :8112
  '';
  services.caddy.virtualHosts."llama.heron-pollux.ts.net".extraConfig = ''
    reverse_proxy :8080
  '';

  services.tailscale.permitCertUid = config.services.caddy.user;

  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
}
