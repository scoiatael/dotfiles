{ config, lib, pkgs, ... }:

{
  # for emacsclient.sh
  home.packages = with pkgs; [ gnused coreutils-full ];
  programs.zsh = {
    enable = true;
    enableCompletion = false;
    shellAliases = {
      hmr =
        "home-manager switch --flake 'path:${config.home.homeDirectory}/dotfiles'";
      nor =
        "doas nixos-rebuild switch --flake 'path:${config.home.homeDirectory}/dotfiles'";
      dnr =
        "darwin-rebuild switch --flake ${config.home.homeDirectory}/dotfiles";
      nix-test =
        "nix-build --keep-failed --expr 'with import <nixpkgs> {}; callPackage ./default.nix {}'";
      g = "git";
      watch = "viddy";
      w = "viddy";
      cat = "bat";
      c = "bat";
      tm = "tmux";
    };
    shellGlobalAliases = { "..." = "../../"; };
    sessionVariables = {
      DOOMLOCALDIR = "$HOME/.emacs.local";
      LSP_USE_PLISTS = "true";
      EDITOR =
        "env PATH=${config.home.homeDirectory}/.nix-profile/bin ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/emacs/emacsclient.sh";
    };
    enableAutosuggestions = true;
    historySubstringSearch = { enable = true; };
    initExtraFirst = ''
      export ZSH_AUTOSUGGEST_MANUAL_REBIND=false
    '';
    initExtra = lib.mkAfter ''
      autoload -U compinit
      compinit -C # assume zcompdump is fresh
      if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
        autoload -U add-zsh-hook
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

      if [[ "$TERM_PROGRAM" = 'tmux' ]]; then
        add-zsh-hook chpwd (){ local SWD="$(print -P '%3~')"; tmux rename-window "''${(@j[/]M)''${(@s[/]M)SWD##*/}#?}$SWD:t" }
      fi

      # TODO: fix on non-Darwin
      path+=("/opt/homebrew/bin/" "$HOME/dotfiles/bin" "$HOME/.emacs.doom/bin")
    '';
    envExtra = ''
      export TMUX_COLORTAG_TAG_ONLY=yes
      export TMUX_COLORTAG_USE_POWERLINE=yes
      export TMUX_COLORTAG_ROUNDED_POWERLINE=yes
    '';
    oh-my-zsh = {
      enable = false;
      plugins = [ "tmux" "gpg-agent" "emacs" ];
      extraConfig = ''
        # ZSH_TMUX_AUTOSTART=true
        ZSH_TMUX_CONFIG=~/.config/tmux/tmux.conf


        ### Fix slowness of pastes with zsh-syntax-highlighting.zsh
        pasteinit() {
          OLD_SELF_INSERT=''${''${(s.:.)widgets[self-insert]}[2,3]}
          zle -N self-insert url-quote-magic # I wonder if you'd need `.url-quote-magic`?
        }

        pastefinish() {
          zle -N self-insert $OLD_SELF_INSERT
        }
        zstyle :bracketed-paste-magic paste-init pasteinit
        zstyle :bracketed-paste-magic paste-finish pastefinish
        ### Fix slowness of pastes
      '';
    };
    plugins = with pkgs; [
      {
        name = "forgit";
        src = zsh-forgit.overrideAttrs {
          postInstall = ''
            substituteInPlace $out/share/zsh/zsh-forgit/forgit.plugin.zsh \
                  --replace-fail "grep" "${lib.getExe pkgs.gnugrep}" \
                  --replace-fail "awk" "${lib.getExe pkgs.gawk}"
          '';
        };
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

  home.activation.updateDotzcompdump = let
    script = pkgs.writeTextFile {
      name = "update.zsh";
      executable = true;
      text = ''
        #! ${pkgs.zsh}/bin/zsh -i
        rm -rf "$HOME"/.zcompdump*
        autoload -U compinit
      '' + (lib.strings.optionalString config.programs.jujutsu.enable ''
        source <(${pkgs.jujutsu}/bin/jj util completion zsh)
        compdef _jj ${pkgs.jujutsu}/bin/jj
      '') + ''
        compinit
      '';
    };
  in config.lib.dag.entryAfter [ "writeBoundary" ] "${script}";
}
