{
  self,
  ...
}:

{
  imports = [
    # Base preset
    ../../modules/home/default.nix
    ../../modules/home/dev/git.nix
    ../../modules/home/editors/neovim.nix

    # Multiplexers and shells
    ../../modules/home/multiplexers/tmux.nix
    ../../modules/home/shells/zsh.nix
    ../../modules/home/editors/emacs.nix
  ];

  home = {
    username = "lukaszczaplinski";
    homeDirectory = "/Users/lukaszczaplinski";
    stateVersion = "22.05";
  };
}
