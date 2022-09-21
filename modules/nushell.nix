{ config, lib, pkgs, ... }:

let
  configPath =
    if pkgs.stdenv.isDarwin then "Library/Application Support/nushell" else "";
in {
  programs.nushell = {
    enable = true;
    configFile = ''
      mkdir ~/.cache/starship
      starship init nu | save ~/.cache/starship/init.nu
      source ~/.cache/starship/init.nu
    '';

    envFile = ''
      # env -> empty for now
    '';
  };
} // lib.optionalAttrs pkgs.stdenv.isDarwin {
  home.file."${configPath}/config.nu".source = "~/.config/nushell/config.nu";
  home.file."${configPath}/env.nu".source = "~/.config/nushell/config.nu";
}
