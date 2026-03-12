{
  pkgs,
  lib,
  ...
}:

{
  imports = [
    ../modules/default.nix
    ../modules/git.nix
    ../modules/editors/neovim.nix
    ../modules/cli.nix
    ../modules/linux.nix
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
