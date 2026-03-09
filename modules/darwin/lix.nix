{
  config,
  lib,
  pkgs,
  ...
}:

{
  nix.package = lib.mkForce pkgs.lix;
}
