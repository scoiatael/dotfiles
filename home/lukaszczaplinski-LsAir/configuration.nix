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

    # Darwin extras
    ../../modules/platform/unfree.nix
    ../../modules/platform/secretive.nix

    # LLM tools
    ../modules/dev/llm.nix

    # Ghostty (commented out)
    #../modules/terminals/ghostty.nix
  ];

  # Ghostty config (commented out)
  #config.ghostty.font-size = 16;

  home = {
    username = "lukaszczaplinski";
    homeDirectory = "/Users/lukaszczaplinski";
    stateVersion = "22.05";
  };
}
