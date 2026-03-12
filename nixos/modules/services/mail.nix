{
  config,
  lib,
  pkgs,
  ...
}:

{
  systemd.services.offlineimap =
    let
      runFile = pkgs.writeShellApplication {
        name = "sync-mail";
        runtimeInputs = with pkgs; [
          offlineimap
          restic
        ];
        text = ''
          offlineimap -c offlineimaprc
          restic backup .
        '';
      };
    in
    {
      description = "offlineimap - mail sync";
      wantedBy = [ "default.target" ];
      restartIfChanged = false;

      environment = {
        RESTIC_CACHE_DIR = "/var/cache/restic-backups-backblaze-Mail";
        RESTIC_REPOSITORY = "b2:macos-Mail-backup";
        B2_ACCOUNT_ID = "0025d465b6563cf000000000a";
      };

      serviceConfig = {
        Type = "oneshot";
        WorkingDirectory = "/home/lukaszczaplinski/Mail";
        Restart = "on-failure";
        User = "lukaszczaplinski";

        EnvironmentFile = "/etc/nixos/secrets/offlineimap";

        ExecStart = lib.getExe runFile;
        CacheDirectoryMode = "0700";
        CacheDirectory = "restic-backups-backblaze-Mail";
        PrivateTmp = true;
      };
    };

  systemd.timers.offlineimap = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "00:05";
      RandomizedDelaySec = "5h";
      Persistent = true;
    };
  };
}
