{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [ htop fish helix doggo tree ];
}
