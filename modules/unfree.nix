{ config, lib, ... }:

{
  options = {
    allowUnfreePackages = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      description = "List of unfree package names to allow";
      example = [ "discord" "steam" "vscode" ];
    };
  };

  config = {
    nixpkgs.config.allowUnfreePredicate = pkg:
      builtins.elem (lib.getName pkg) config.allowUnfreePackages;
  };
}
