{ config, lib, pkgs, ... }:

{
  systemd.services = lib.mapAttrs' (name: config:
    lib.nameValuePair "tailscale-serve-${name}" {
      description = "tailscale serve - svc:${name}";
      wantedBy = [ "default.target" ];

      serviceConfig = {
        Type = "oneshot";
        Restart = "on-failure";
        ExecStart =
          "${pkgs.tailscale}/bin/tailscale serve --service=svc:${name} ${
            toString config.port
          }";
      };
    }) {
      parrhasius = { port = 4567; };
      yarr = { port = 7070; };
      scrutiny = { port = 8053; };
      jellyfin = { port = 8096; };
      influxdb = { port = 8086; };
    };
}
