{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [ wofi (callPackage ../packages/gtklock { }) ];

  xdg.configFile."newm/config.py".source = ../config/newm/config.py;
}
