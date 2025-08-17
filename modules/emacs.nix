{ doomemacs, ... }:
{ config, lib, pkgs, ... }:
let emacsPackage = config.programs.emacs.finalPackage;
in {
  home.packages = with pkgs;
    [
      #recutils
      nixfmt-classic
      sqlite
      clang
      # TODO: https://github.com/aca/emmet-ls
      zstd.bin
      pass

      vtsls # LSP for JS
      rufo # formatter for Ruby
      gopls # LSP for Go

      victor-mono
      nerd-fonts.symbols-only

      babashka

      # notmuch # mail

      nuspell # spell checking

      # for emacsclient.sh
      gnused
      uutils-coreutils-noprefix

      # for thumbnails in deer
      vips
    ] ++ (with pkgs.hunspellDicts; [ pl_PL en_GB-ise ]);

  fonts.fontconfig.enable = true; # required to autoload fonts from packages

  # programs.afew.enable = true;

  programs.zsh.sessionVariables.NOTMUCH_CONFIG =
    "${config.home.homeDirectory}/Mail/notmuch-config";

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm epkgs.jinx ];
  };
  programs.zsh.sessionVariables = {
    DOOMLOCALDIR = "$HOME/.emacs.local";
    LSP_USE_PLISTS = "true";
    EDITOR =
      "env PATH=${config.home.homeDirectory}/.nix-profile/bin ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/emacs/emacsclient.sh";
  };

  # avoid copying into /nix/store to allow easy changes
  # home.file.".config/doom".source = config.lib.file.mkOutOfStoreSymlink "~/dotfiles/config/doom";
  home.activation.linkDoomConfig =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      test -d ~/.config || mkdir ~/.config
      test -d ~/.config/doom || ln -sf ~/dotfiles/config/doom ~/.config/doom

      export DOOMLOCALDIR="~/.emacs.local/"
      export EMACSDIR="~/.emacs.doom/"
      export PATH=${emacsPackage}/bin/:${pkgs.git}/bin/:${pkgs.openssh}/bin/:$PATH
      ${doomemacs}/bin/doom sync --force # Suppress prompts by auto-accepting their consequences.
    '';

  home.file.".emacs.doom".source = doomemacs;
  home.file.".emacs.d/early-init.el".text = ''
    (setq envrc-direnv-executable "${pkgs.direnv}/bin/direnv")
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "~/.emacs.local/")))
    (setenv "EMACSDIR" (expand-file-name (file-name-as-directory "~/.emacs.doom/")))
    (setenv "SSH_AUTH_SOCK" (expand-file-name "S.gpg-agent.ssh" (file-name-as-directory "~/.gnupg/")))
    (load (concat (expand-file-name (file-name-as-directory "${doomemacs}")) "early-init.el") nil 'nomessage)
  '';
  home.file.".config/enchant/hunspell/".source = pkgs.symlinkJoin {
    name = "hunspell-dicts";
    paths = [
      "${pkgs.hunspellDicts.pl_PL}/share/hunspell/"
      "${pkgs.hunspellDicts.en_GB-ise}/share/hunspell/"
    ];
  };

  programs.zsh.initContent = lib.mkAfter ''
    if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
      add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

      vterm_printf() {
          if [ -n "$TMUX" ] && ([ "$TERM" = "tmux" ] || [ "$TERM" = "screen" ]); then
              # Tell tmux to pass the escape sequences through
              printf "\ePtmux;\e\e]%s\007\e\\" "$1"
          elif [ "$TERM" = "screen" ]; then
              # GNU screen (screen, screen-256color, screen-256color-bce)
              printf "\eP\e]%s\007\e\\" "$1"
          else
              printf "\e]%s\e\\" "$1"
          fi
      }

      alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

      vterm_prompt_end() {
          vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
      }

      vterm_cmd() {
          local vterm_elisp
          vterm_elisp=""
          while [ $# -gt 0 ]; do
              vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
              shift
          done
          vterm_printf "51;E$vterm_elisp"
      }

      find_file() {
          vterm_cmd find-file "$(realpath "$@")"
      }

      say() {
          vterm_cmd message "%s" "$*"
      }

      setopt PROMPT_SUBST
      PROMPT=$PROMPT' %{$(vterm_prompt_end)%}'

      alias vi=find_file
    fi
  '';
  programs.zsh.oh-my-zsh.plugins = [ "emacs" ];
}
