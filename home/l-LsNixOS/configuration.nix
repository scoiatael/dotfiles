{
  self,
  walker,
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

    # Dev preset
    ../../modules/home/multiplexers/tmux.nix
    ../../modules/home/shells/zsh.nix
    ../../modules/home/editors/emacs.nix
    ../../modules/home/terminals/wezterm.nix
    ../../modules/home/home-manager.nix
    ../../modules/home/comma.nix

    # Linux extras
    ../../modules/platform/linux.nix
    ../../modules/platform/electron.nix

    # Walker
    walker.homeManagerModules.default
    ../../modules/platform/walker.nix
    ../../modules/home/terminals/ghostty.nix
  ];

  programs.git.settings.user = {
    email = "lukasz@wooting.io";
    name = "Lukas Czaplinski";
    signingkey = "E871295C0EFA7DBFA9E673CC7135745D2C62273D";
  };

  home = {
    username = "l";
    homeDirectory = "/home/l";
    stateVersion = "24.11";
  };
}
