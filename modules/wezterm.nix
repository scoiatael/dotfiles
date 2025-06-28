{ lib, pkgs, ... }:

{
  programs.wezterm = {
    enable = true;
    extraConfig = lib.mkMerge [
      (builtins.readFile ../config/wezterm/config.lua)
      (lib.strings.optionalString pkgs.stdenv.isLinux ''
        config.dpi=72
      '')
      (builtins.readFile ../config/wezterm/modal.lua)
      (builtins.readFile ../config/wezterm/smart-splits.lua)
      ''
        return config
      ''
    ];
  };
}
