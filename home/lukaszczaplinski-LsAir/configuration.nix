{
  imports = [
    ../modules/default.nix
    ../modules/git.nix
    ../modules/editors/neovim.nix
    ../modules/cli.nix
    ../modules/multiplexers/tmux.nix
    ../modules/shells/zsh.nix
    ../modules/editors/emacs.nix
    ../modules/terminals/wezterm.nix
    ../modules/home-manager.nix
    ../modules/comma.nix
    ../modules/secretive.nix
    ../modules/llm.nix
  ];

  home = {
    username = "lukaszczaplinski";
    homeDirectory = "/Users/lukaszczaplinski";
    stateVersion = "22.05";
  };
}
