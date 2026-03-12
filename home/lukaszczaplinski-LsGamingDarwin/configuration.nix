{
  self,
  ...
}:

{
  imports = [
    # Base preset
    ../modules/default.nix
    ../modules/dev/git.nix
    ../modules/editors/neovim.nix

    # Multiplexers and shells
    ../modules/multiplexers/tmux.nix
    ../modules/shells/zsh.nix
    ../modules/editors/emacs.nix
  ];

  home = {
    username = "lukaszczaplinski";
    homeDirectory = "/Users/lukaszczaplinski";
    stateVersion = "22.05";
  };
}
