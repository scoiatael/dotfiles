{ home-manager, ... }:
{ config, pkgs, ... }:

{
  home.packages = [
    #  binary is missing on PATH when using nix-darwin
    #  due to https://github.com/nix-community/home-manager/blob/a46e702093a5c46e192243edbd977d5749e7f294/modules/programs/home-manager.nix#L30
    (pkgs.callPackage "${home-manager}/home-manager" {
      inherit (config.programs.home-manager) path;
    })
  ];
}
