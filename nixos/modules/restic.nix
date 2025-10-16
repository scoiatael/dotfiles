{ config, lib, pkgs, ... }:

{
  users.users.restic = { isNormalUser = true; };

  security.wrappers.restic = {
    source = "${pkgs.restic.out}/bin/restic";
    owner = "restic";
    group = "users";
    permissions = "u=rwx,g=,o=";
    capabilities = "cap_dac_read_search=+ep";
  };

  services.restic.backups = let
    package = pkgs.writeShellScriptBin "restic" ''
      exec /run/wrappers/bin/restic "$@"
    '';
    user = "restic";
  in {
    Mail = {
      inherit user;
      inherit package;
      paths = [ "/home/lukaszczaplinski/Mail" ];
      repository = "/srv/nfs/backups/Mail";
      passwordFile = "/etc/nixos/secrets/restic-password";
      initialize = true;
    };
    yarr = {
      user = "root"; # Getting r/w on /var/lib/yarr/storage.db is freaking hard
      inherit package;
      command =
        [ "${lib.getExe pkgs.sqlite}" "/var/lib/yarr/storage.db" ".dump" ];
      repository = "/srv/nfs/backups/yarr";
      passwordFile = "/etc/nixos/secrets/restic-password";
      initialize = true;
    };
  };
}
