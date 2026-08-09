{
  config,
  lib,
  pkgs,
  ...
}:

{
  users.users.valheim = {
    isSystemUser = true;
    shell = "/run/current-system/sw/bin/nologin";
    createHome = true;
    home = "/var/lib/valheim";
  };
  users.users.restic.group = "restic";
  users.groups.restic = { };
  # docker run -d \
  #     --name valheim-server \
  #     --cap-add=sys_nice \
  #     --stop-timeout 120 \
  #     -p 2456-2457:2456-2457/udp \
  #     -v $HOME/valheim-server/config:/config \
  #     -v $HOME/valheim-server/data:/opt/valheim \
  #     -e SERVER_NAME="My Server" \
  #     -e WORLD_NAME="Neotopia" \
  #     -e SERVER_PASS="secret" \
  #     ghcr.io/community-valheim-tools/valheim-server
  virtualisation.oci-containers.containers.valheim-server = {
    image = "ghcr.io/community-valheim-tools/valheim-server";
    environmentFiles = [ config.sops.secrets.valheim-server-env.path ];
    volumes = [
      "${config.users.users.valheim.home}/config:/config"
      "${config.users.users.valheim.home}/data:/opt/valheim"
    ];
    ports = [
      "127.0.0.1:2456-2457:2456-2457/udp"
    ];
    capabilities = {
      SYS_NICE = true;
    };
    extraOptions = [
      "--stop-timeout 120"
    ];
  };
}
