{
  config,
  lib,
  pkgs,
  ...
}:

let
  # Combat: veryeasy, easy, hard, veryhard
  # DeathPenalty: casual, veryeasy, easy, hard, hardcore
  # Resources: muchless, less, more, muchmore, most
  # Raids: none, muchless, less, more, muchmore
  # Portals: casual, hard, veryhard
  modifiers = {
    Combat = "hard";
    DeathPenalty = "casual";
    Resources = "most";
    Raids = "muchless";
    Portals = "casual";
  };
  serverArgs = [
    "-preset"
    "normal"
  ]
  ++ (builtins.concatLists (
    lib.mapAttrsToList (mod: val: [
      "-modifier"
      mod
      val
    ]) modifiers
  ));
  serverEnv = pkgs.writers.writeText "valheim-server-args" ''
    SERVER_ARGS=${lib.escapeShellArgs serverArgs}
  '';
in
{
  users.users.valheim = {
    isSystemUser = true;
    shell = "/run/current-system/sw/bin/nologin";
    createHome = true;
    home = "/var/lib/valheim";
  };
  users.users.valheim.group = config.users.groups.valheim.name;
  users.groups.valheim = { };
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
  systemd.tmpfiles.rules = [
    "d ${config.users.users.valheim.home}/config 0700 ${config.users.users.valheim.name} ${config.users.groups.valheim.name} -"
    "d ${config.users.users.valheim.home}/data 0700 ${config.users.users.valheim.name} ${config.users.groups.valheim.name} -"
  ];
  virtualisation.oci-containers.containers.valheim-server = {
    image = "ghcr.io/community-valheim-tools/valheim-server";
    environmentFiles = [
      config.sops.secrets.valheim-server-env.path
      serverEnv
    ];
    volumes = [
      "${config.users.users.valheim.home}/config:/config"
      "${config.users.users.valheim.home}/data:/opt/valheim"
    ];
    ports = [
      "2456-2457:2456-2457/udp"
    ];
    capabilities = {
      SYS_NICE = true;
    };
    extraOptions = [
      "--stop-timeout=120"
    ];
  };
  services.restic.backups.b2-valheim-server = {
    environmentFile = config.sops.secrets.backblaze-valheim-env.path;
    passwordFile = config.sops.secrets.restic-password.path;
    paths = [ "${config.users.users.valheim.home}/config" ];
    repository = "b2:sd-161581-valheim";
    initialize = true;
  };
}
