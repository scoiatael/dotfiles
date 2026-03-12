{ config, lib, pkgs, ... }:

{
  nix.gc = {
    automatic = true;
    randomizedDelaySec = "15min";
    persistent = true;
  };

  nix.nixPath = [ "nixpkgs=${pkgs.path}" ];
  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
