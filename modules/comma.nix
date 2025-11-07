attrs:
{ config, lib, pkgs, ... }:

{
  imports = [
    attrs.nix-index-database.homeModules.nix-index
    { programs.nix-index-database.comma.enable = true; }
  ];
}
