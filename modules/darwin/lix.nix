{
  config,
  lib,
  pkgs,
  lix-module,
  ...
}:

{
  imports = [
    lix-module.darwinModules.default
  ];
  nix.package = lib.mkForce pkgs.lix;
}
