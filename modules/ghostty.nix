{ config, lib, pkgs, ... }:

{
  options.ghostty.font-size = lib.mkOption {
    type = lib.types.int;
    default = 9;
  };
  config = {
    home.packages = [ pkgs.ghostty ];

    home.file.".config/ghostty/config".text = ''
      theme = catppuccin-frappe
      font-size = ${builtins.toString config.ghostty.font-size}
    '';
  };
}
