{
  config,
  lib,
  pkgs,
  ...
}:

{
  users.users.restic = {
    isNormalUser = true;
  };

  security.wrappers.restic = {
    source = "${pkgs.restic.out}/bin/restic";
    owner = "restic";
    group = "users";
    permissions = "u=rwx,g=,o=";
    capabilities = "cap_dac_read_search=+ep";
  };

  services.restic.backups =
    let
      package = pkgs.writeShellScriptBin "restic" ''
        exec /run/wrappers/bin/restic "$@"
      '';
      user = "restic";
      passwordFile = config.sops.secrets.restic-password.path;
    in
    {
      b2-Pictures = {
        inherit user;
        inherit package;
        inherit passwordFile;
        environmentFile = config.sops.secrets.backblaze-framework-srv-nfs-env.path;
        paths = [
          "/srv/nfs/Pictures"
        ];
        repository = "b2:framework-srv-nfs";
      };
      immich = {
        inherit user;
        inherit package;
        inherit passwordFile;
        paths = [ config.services.immich.mediaLocation ];
        repository = "/srv/nfs/backups/immich";
      };
      Mail = {
        inherit user;
        inherit package;
        inherit passwordFile;
        paths = [ "/home/lukaszczaplinski/Mail" ];
        repository = "/srv/nfs/backups/Mail";
      };
      org-roam = {
        inherit user;
        inherit package;
        inherit passwordFile;
        paths = [ "/home/lukaszczaplinski/org/roam" ];
        repository = "/srv/nfs/backups/org-roam";
      };
      yarr = {
        user = "root"; # Getting r/w on /var/lib/yarr/storage.db is freaking hard
        inherit package;
        inherit passwordFile;
        command = [
          "${lib.getExe pkgs.sqlite}"
          "/var/lib/yarr/storage.db"
          ".dump"
        ];
        repository = "/srv/nfs/backups/yarr";
      };
    };
}
