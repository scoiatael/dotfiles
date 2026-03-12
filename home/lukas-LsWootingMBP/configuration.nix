{
  imports = [
    ../../modules/platform/unfree.nix
    ../../modules/home/default.nix
    ../../modules/home/cli.nix
    ../../modules/home/home-manager.nix
    ../../modules/home/dev/git.nix
    ../../modules/home/multiplexers/tmux.nix
    ../../modules/home/shells/zsh.nix
    ../../modules/home/editors/emacs.nix
    ../../modules/home/terminals/wezterm.nix
    ../../modules/home/dev/llm.nix
    ../../modules/home/editors/neovim.nix
    ../../modules/home/comma.nix
    ../../modules/git/graphite.nix
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
