{ dirsummary, ... }:
{ config, lib, pkgs, ... }:

{
  # for emacsclient.sh
  home.packages = with pkgs; [ gnused coreutils-full ];
  programs.zsh = {
    enable = true;
    enableCompletion = false;
    shellAliases = let nh = lib.getExe pkgs.nh;
    in {
      hmr =
        "${nh} home switch 'path:${config.home.homeDirectory}/dotfiles' -b bp.$(date --iso-8601)";
      nor =
        "doas ${nh} os switch -R 'path:${config.home.homeDirectory}/dotfiles'";
      dnr =
        "darwin-rebuild switch --flake ${config.home.homeDirectory}/dotfiles";
      nix-test =
        "nix-build --keep-failed --expr 'with import <nixpkgs> {}; callPackage ./default.nix {}'";
      nix-test-python =
        "nix-build --keep-failed --expr 'let pkgs = import <nixpkgs> {}; in with pkgs; with python3Packages; callPackage ./default.nix {}'";
      g = "git";
      watch = "viddy";
      w = "viddy";
      cat = "bat";
      c = "bat";
      tm = "tmux";
      man = "batman --paging=always";
      n = "nix";
      ns = "nix-shell";
      nf = "nix flake";
      nix-repl = "nix repl -f '<nixpkgs>'";
      zsh_stats = ''
        atuin history list --cmd-only | awk '{CMD[$1]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n10'';
      nix-tree = "nix run github:utdemir/nix-tree -- $DEVENV_PROFILE";
      nix-dust =
        "nix path-info --size --recursive -h $DEVENV_PROFILE | sort -hk2";
      nix-du = "nix path-info -Sh $DEVENV_PROFILE";
      d = "direnv";
      gpg-fpr = "gpg -K --with-colons | grep fpr | head -n 1 | ${
          lib.getExe pkgs.choose
        } -f : -1";
      gpg-quick-expire-extend = ''
        gpg --quick-set-expire "$(gpg-fpr)" 3m '*'; gpg --quick-set-expire "$(gpg-fpr)" 3m'';
      random = ''
        ruby -r securerandom -e "puts SecureRandom.hex(ARGV.first&.to_i || 32)" "''${@}"'';
    };
    shellGlobalAliases = { } // (lib.lists.foldl' (acc: op:
      let
        name = "." + lib.concatStrings (lib.replicate op ".");
        cmd = lib.concatStrings (lib.replicate op "../");
      in acc // { "${name}" = cmd; }) { } (lib.range 2 4));
    sessionVariables = {
      DOOMLOCALDIR = "$HOME/.emacs.local";
      LSP_USE_PLISTS = "true";
      EDITOR =
        "env PATH=${config.home.homeDirectory}/.nix-profile/bin ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/emacs/emacsclient.sh";
    };
    autosuggestion = { enable = true; };
    historySubstringSearch = { enable = true; };
    initExtraFirst = ''
      export ZSH_AUTOSUGGEST_MANUAL_REBIND=false
      export FZF_CTRL_T_COMMAND="fd --type f --hidden --follow --exclude .git --exclude .devenv --exclude .direnv"
    '';
    initExtra = let summary = dirsummary.packages.${pkgs.stdenv.system}.default;
    in lib.mkAfter ''
      autoload -U compinit
      compinit -C # assume zcompdump is fresh

      bindkey "" history-beginning-search-forward
      bindkey "" history-beginning-search-backward
      bindkey '^W' backward-delete-word

      autoload -Uz add-zsh-hook
      # https://github.com/rothgar/mastering-zsh/blob/master/docs/config/hooks.md#add-function-to-hook
      do-ls() {${summary}/bin/dir-summary $PWD;}

      # add do-ls to chpwd hook
      add-zsh-hook chpwd do-ls

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


        FAST_HIGHLIGHT[use_brackets]=1

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
          postInstall = let
            grep = lib.getExe pkgs.gnugrep;
            awk = lib.getExe pkgs.gawk;
          in ''
            substituteInPlace $out/share/zsh/zsh-forgit/forgit.plugin.zsh \
                  --replace-fail "grep" "${grep}" \
                  --replace-fail "awk" "${awk}"
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
      # {
      #   name = "syntax-hightlighing";
      #   src = zsh-syntax-highlighting;
      #   file = "share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh";
      # }
      {
        name = "fast-syntax-hightlighing";
        src = zsh-fast-syntax-highlighting;
        file = "share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh";
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
