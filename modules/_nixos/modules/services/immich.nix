{ config, lib, ... }:

{
  services.immich = {
    enable = true;
    host = "127.0.0.1";
    mediaLocation = "/srv/nfs/Pictures/immich";
  };
  systemd.services.tailscale-funnel-immich = {
    description = "Tailscale Funnel forwarding for Immich";

    # https://old.reddit.com/r/Tailscale/comments/ubk9mo/systemd_how_do_get_something_to_run_if_tailscale/jia3pwn/
    after = [
      "sys-subsystem-net-devices-tailscale0.device"
      "tailscaled.service"
      "immich-server.service"
    ];
    wantedBy = [ "multi-user.target" ];

    serviceConfig.ExecStart = "${lib.getExe config.services.tailscale.package} funnel ${builtins.toString config.services.immich.port}";
  };
}
