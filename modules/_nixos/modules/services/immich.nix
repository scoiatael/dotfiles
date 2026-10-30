{

  # - Immich 2.x.x will not receive further updates. Immich 3.x.x is available in NixOS 26.11 (unstable at the time of writing)
  # - CVE-2026-59258
  # - CVE-2026-82272
  # TODO: update to immich 3.x
  nixpkgs.config.permittedInsecurePackages = [
    "immich-2.7.5"
  ];
  services.immich = {
    enable = false;
    host = "127.0.0.1";
    mediaLocation = "/srv/nfs/Pictures/immich";
  };
  # TODO: re-enable after update to immich 3.x
  # systemd.services.tailscale-funnel-immich = {
  #   description = "Tailscale Funnel forwarding for Immich";

  #   # https://old.reddit.com/r/Tailscale/comments/ubk9mo/systemd_how_do_get_something_to_run_if_tailscale/jia3pwn/
  #   after = [
  #     "sys-subsystem-net-devices-tailscale0.device"
  #     "tailscaled.service"
  #     "immich-server.service"
  #   ];
  #   wantedBy = [ "multi-user.target" ];

  #   serviceConfig.ExecStart = "${lib.getExe config.services.tailscale.package} funnel ${builtins.toString config.services.immich.port}";
  # };
}
