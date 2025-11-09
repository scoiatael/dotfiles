{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    htop
    fish
    helix
    tree # [[id:1b6a2a6d-c2ca-4cb6-8124-163b67f82570][tree]]
    dust
    tailspin # [[id:8042e9e2-b2cb-4c24-bd34-64cc6b56aac7][tailspin]]
    nix-tree
    doggo # [[id:6ce78a90-39c3-4065-bee8-344ece1c702b][doggo]]
    wezterm # for wezterm ssh
  ];
}
