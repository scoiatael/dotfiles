{
  pkgs,
  lib,
  ...
}:

{
  imports = [
    # Base preset
    ../modules/default.nix
    ../modules/dev/git.nix
    ../modules/editors/neovim.nix

    # CLI preset
    ../modules/cli.nix

    # Linux extras
    ../../modules/platform/linux.nix

    # Multiplexers
    ../modules/multiplexers/zellij.nix
    ../modules/multiplexers/tmux.nix
    ../modules/shells/zsh.nix
  ];

  programs.zsh.sessionVariables = {
    EDITOR = lib.getExe pkgs.neovim;
  };

  home = {
    username = "lukaszczaplinski";
    homeDirectory = "/home/lukaszczaplinski";
    stateVersion = "22.11";
  };
}
