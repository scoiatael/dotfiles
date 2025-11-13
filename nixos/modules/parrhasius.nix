{ config, lib, pkgs, ... }:

{
  systemd.services.parrhasius = {
    description = "Parrhasius";
    wantedBy = [ "default.target" ];

    serviceConfig = {
      Type = "simple";
      WorkingDirectory = "/home/lukaszczaplinski/Documents/parrhasius";
      Restart = "on-failure";
      User = "lukaszczaplinski";

      Environment = [
        "SERVE=/srv/nfs/downloads/parrhasius.db/"
        "RAILS_ENV=production"
        "PATH=/run/wrappers/bin:/home/lukaszczaplinski/.nix-profile/bin:/nix/profile/bin:/home/lukaszczaplinski/.local/state/nix/profile/bin:/etc/profiles/per-user/lukaszczaplinski/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin"
        "DEVENV_ROOT=/home/lukaszczaplinski/Documents/parrhasius"
      ];

      ExecStartPre = [
        "${pkgs.bash}/bin/bash -c '${pkgs.nix}/bin/nix develop --impure -c bundle install'"
        "${pkgs.bash}/bin/bash -c '${pkgs.nix}/bin/nix develop --impure -c rake build'"
      ];

      ExecStart =
        "${pkgs.bash}/bin/bash -c '${pkgs.nix}/bin/nix develop --impure -c bundle exec puma --bind=tcp://0.0.0.0:4567 -C config/puma.rb'";
    };
  };
}
