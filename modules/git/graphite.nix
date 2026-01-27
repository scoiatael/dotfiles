{ config, lib, pkgs, ... }:

{
  allowUnfreePackages = [ "graphite-cli" ];
  home.packages = [ pkgs.graphite-cli ];
}
