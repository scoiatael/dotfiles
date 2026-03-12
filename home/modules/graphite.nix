{
  pkgs,
  ...
}:

{
  imports = [ ../../.nix/unfree.nix ];
  allowUnfreePackages = [ "graphite-cli" ];
  home.packages = [ pkgs.graphite-cli ];
}
