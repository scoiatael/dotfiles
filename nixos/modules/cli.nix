{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    htop
    fish
    helix
    doggo
    tree
    dust
    tailspin
    nix-tree
    doggo
  ];
}
