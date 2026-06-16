let
  mkBackupCommand =
    pkgs:
    let
      date = "${pkgs.coreutils}/bin/date";
    in
    pkgs.writeShellScriptBin "mv-backup" ''
      mv "$1" "$1"."$(${date} --iso-8601=s)".bak
    '';
  nixos = { pkgs, ... }: {
    home-manager.backupCommand = mkBackupCommand pkgs;
  };
  darwin = nixos;
in
{
  den.batteries.mkBackupCommand = {
    includes = [
      (
        { host }:
        {
          name = "mkBackupCommand/${host.name}";
          inherit nixos darwin;
        }
      )
    ];
  };
}
