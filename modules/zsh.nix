{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    shellAliases = {
      hmr =
        "home-manager switch --flake 'path:${config.home.homeDirectory}/dotfiles'";
      nix-test =
        "nix-build --keep-failed --expr 'with import <nixpkgs> {}; callPackage ./default.nix {}'";
    };
    shellGlobalAliases = { "..." = "../../"; };
    sessionVariables = {
      DOOMLOCALDIR = "$HOME/.emacs.local";
      LSP_USE_PLISTS = "true";
    };
    enableAutosuggestions = true;
    historySubstringSearch = { enable = true; };
    oh-my-zsh = {
      enable = true;
      plugins = [ "tmux" "emacs" "gpg-agent" ];
      extraConfig = ''
        ZSH_TMUX_AUTOSTART=true
        ZSH_TMUX_CONFIG=~/.config/tmux/tmux.conf

        export TMUX_COLORTAG_TAG_ONLY=yes
        export TMUX_COLORTAG_USE_POWERLINE=yes
        export TMUX_COLORTAG_ROUNDED_POWERLINE=yes
      '';
    };
    plugins = with pkgs; [
      {
        name = "forgit";
        src = zsh-forgit;
        file = "share/zsh/zsh-forgit/forgit.plugin.zsh";
      }
      {
        name = "edit";
        src = zsh-edit;
        file = "share/zsh/zsh-edit/zsh-edit.plugin.zsh";
      }
      {
        name = "autopair";
        src = zsh-autopair;
        file = "share/zsh/zsh-autopair/autopair.zsh";
      }
      {
        name = "fzf-tab";
        src = zsh-fzf-tab;
        file = "share/fzf-tab/fzf-tab.plugin.zsh";
      }
      {
        name = "you-should-use";
        src = zsh-you-should-use;
        file = "share/zsh/plugins/you-should-use/you-should-use.plugin.zsh";
      }
      {
        name = "syntax-hightlighing";
        src = zsh-syntax-highlighting;
        file = "share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh";
      }
    ];
  };
  programs.starship.enableZshIntegration = true;
}
