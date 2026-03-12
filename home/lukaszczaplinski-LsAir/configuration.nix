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

    # Darwin extras
    ../../modules/platform/unfree.nix
    ../../modules/platform/secretive.nix

    # LLM tools
    ../../modules/home/dev/llm.nix

    # Ghostty (commented out)
    #../../modules/home/terminals/ghostty.nix
  ];

  # Ghostty config (commented out)
  #config.ghostty.font-size = 16;

  home = {
    username = "lukaszczaplinski";
    homeDirectory = "/Users/lukaszczaplinski";
    stateVersion = "22.05";
  };
}
