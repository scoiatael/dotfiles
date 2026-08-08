{
  pkgs,
  ...
}:

{
  systemd.services.mount_nfs = {
    path = [
      pkgs.lvm2
      pkgs.cryptsetup
    ];
    script = ''
      systemd-cryptsetup attach cryptHdd_mirror1 /dev/disk/by-uuid/185303df-9a82-49b7-936a-d3114f66f37c
      systemd-cryptsetup attach cryptHdd_mirror2 /dev/disk/by-uuid/1c9c6c95-effb-4e8f-8df0-bb024bd81502
      systemd-cryptsetup attach cryptHdd_mirror3 /dev/disk/by-uuid/c0ec16c5-82bf-4773-bdea-30c81e347d37
      vgchange --refresh
      /run/wrappers/bin/mount /dev/mapper/SrvVG-nfs /srv/nfs
    '';
    serviceConfig = {
      Type = "idle";
    };
    wantedBy = [
      "immich-server.service"
      "deluged.service"
    ];
  };
}
