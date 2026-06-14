{ config, lib, pkgs, ... }:

{
  users.users.remotebuild = {
    isSystemUser = true;
    group = "remotebuild";
    useDefaultShell = true;
  };

  users.groups.remotebuild = { };

  nix = {
    nrBuildUsers = 64;
    settings = {
      trusted-users = [ "remotebuild" ];

      min-free = 10 * 1024 * 1024;
      max-free = 200 * 1024 * 1024;

      max-jobs = "auto";
      cores = 0;
    };
  };

  systemd.services.nix-daemon.serviceConfig = {
    MemoryAccounting = true;
    MemoryMax = "90%";
    OOMScoreAdjust = 500;
  };
}
