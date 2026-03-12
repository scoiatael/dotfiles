{
  imports = [
    ../modules/default.nix
    ../modules/cli.nix
    ../modules/home-manager.nix
    ../modules/git.nix
    ../modules/multiplexers/tmux.nix
    ../modules/shells/zsh.nix
    ../modules/editors/emacs.nix
    ../modules/terminals/wezterm.nix
    ../modules/llm.nix
    ../modules/editors/neovim.nix
    ../modules/comma.nix
    ../modules/graphite.nix
  ];

  programs.git.settings.user = {
    email = "lukasz@wooting.io";
    name = "Lukas Czaplinski";
    signingkey = "E871295C0EFA7DBFA9E673CC7135745D2C62273D";
  };

  home = {
    username = "lukas";
    homeDirectory = "/Users/lukas";
    stateVersion = "22.05";
  };
}
