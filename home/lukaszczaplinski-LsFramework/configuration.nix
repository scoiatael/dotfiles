{
  pkgs,
  lib,
  ...
}:

{
  imports = [
    # Base preset
    ../../modules/home/default.nix
    ../../modules/home/dev/git.nix
    ../../modules/home/editors/neovim.nix

    # CLI preset
    ../../modules/home/cli.nix

    # Linux extras
    ../../modules/platform/linux.nix

    # Multiplexers
    ../../modules/home/multiplexers/zellij.nix
    ../../modules/home/multiplexers/tmux.nix
    ../../modules/home/shells/zsh.nix
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
