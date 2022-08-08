{ config, lib, pkgs, persway, ... }:
# https://github.com/johnae/nixos-configuration/blob/b10bedaf0dc66bba527a2d825fe5f4b687a1cbe2/home/sway.nix
let
  swayservice = Description: ExecStart: {
    Unit = {
      inherit Description;
      After = "sway-session.target";
      BindsTo = "sway-session.target";
    };

    Service = {
      Type = "simple";
      inherit ExecStart;
    };

    Install = { WantedBy = [ "sway-session.target" ]; };
  };
in {
  nixpkgs.overlays = [ persway.overlays.default ];

  systemd.user.services = {
    persway =
      swayservice "Small Sway IPC Deamon" "${pkgs.persway}/bin/persway -aw";
  };
}
