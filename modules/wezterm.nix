{ config, lib, pkgs, ... }:

{
  programs.wezterm = { enable = true; };
  xdg.configFile."wezterm/wezterm.lua".source = ../config/wezterm.lua;
}
