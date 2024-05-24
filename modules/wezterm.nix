{ config, lib, pkgs, ... }:

{
  programs.wezterm = { enable = true; };
  xdg.configFile."wezterm/wezterm.lua".source = ../config/wezterm.lua;
  # programs.zsh.initExtra = ''
  #   autoload -U add-zsh-hook
  #   add-zsh-hook -Uz chpwd (){ __wezterm_set_user_var PWD "$PWD" }
  # '';
}
