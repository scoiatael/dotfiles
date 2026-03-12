{
  self,
  walker,
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

    # Dev preset
    ../modules/multiplexers/tmux.nix
    ../modules/shells/zsh.nix
    ../modules/editors/emacs.nix
    ../modules/terminals/wezterm.nix
    ../modules/home-manager.nix
    ../modules/comma.nix

    # Linux extras
    ../../modules/platform/linux.nix
    ../../modules/platform/electron.nix

    # Walker
    walker.homeManagerModules.default
    ../../modules/platform/walker.nix
    ../modules/terminals/ghostty.nix
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
