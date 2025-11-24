{ ... }:
{ config, lib, pkgs, ... }:
let dir-summary = pkgs.callPackage ../packages/dir-summary { };
in {
  home.packages = [ dir-summary ];
  programs.zsh = {
    enable = true;
    enableCompletion = false;
    shellAliases = let
      nh = lib.getExe pkgs.nh;
      date = "${pkgs.coreutils}/bin/date";
    in {
      gpg-wake = "echo 'foo' | gpg -e -r $(gpg-fpr) --armour | gpg -d";
      xzf = "ouch decompress";
      lzf = "ouch list";
      wssh = ''
        (){ local TAB; TAB="$(wezterm  cli spawn --domain-name SSH:$1)"; test $? -eq 0 && wezterm cli set-tab-title --tab-id $TAB "[ssh] $1" && exit  }'';
      imgcat = "wezterm imgcat";
      da = "direnv allow";
      jl = ''
        just --list --list-heading="" | fzf --reverse | choose 0 | xargs just
      '';
      poi =
        "gh poi --dry-run; read -q 'ok? [Y/y]' && gh poi || echo 'aborted by prompt' ";
      hmr =
        "${nh} home switch 'path:${config.home.homeDirectory}/dotfiles' -b bp.$(${date} --iso-8601)";
      nor =
        "doas ${nh} os switch -R 'path:${config.home.homeDirectory}/dotfiles'";
      dnr = "${nh} darwin switch 'path:${config.home.homeDirectory}/dotfiles'";
      nix-test =
        "nix-build --keep-failed --expr 'with import <nixpkgs> {}; callPackage ./default.nix {}'";
      nix-test-python =
        "nix-build --keep-failed --expr 'let pkgs = import <nixpkgs> {}; in with pkgs; with python3Packages; callPackage ./default.nix {}'";
      nix-tree-devshell =
        "nix-tree --derivation '.#devShells.${pkgs.stdenv.hostPlatform.system}.default'";
      # WARNING: The convert command is deprecated in IMv7, use "magick" instead of "convert" or "magick convert"
      convert = "magick";
      g = "git";
      gb = "git checkout $(g br --color=always | fzf --ansi | choose 0)";
      watch = "viddy";
      w = "viddy";
      cat = "bat";
      c = "bat";
      man = "batman --paging=always";
      n = "nix";
      ns = "nix-shell";
      nf = "nix flake";
      nr = "nix run";
      nix-repl = "nix repl -f '<nixpkgs>'";
      zsh_stats = ''
        atuin history list --cmd-only | awk '{CMD[$1]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n10'';
      nix-dust =
        "nix path-info --size --recursive -h $DEVENV_PROFILE | sort -hk2";
      nix-du = "nix path-info -Sh $DEVENV_PROFILE";
      d = "direnv";
      gpg-fpr = "gpg -K --with-colons | grep fpr | head -n 1 | ${
          lib.getExe pkgs.choose
        } -f : -1";
      gpg-subkeys = ''
        gpg --list-secret-keys --with-subkey --with-colons | grep fpr | ${
          lib.getExe pkgs.choose
        } -f : -1 | grep -v "$(gpg-fpr)" | xargs echo
      '';
      gpg-quick-expire-extend = ''
        gpg --quick-set-expire "$(gpg-fpr)" 3m && gpg --quick-set-expire "$(gpg-fpr)" 3m $(gpg-subkeys)'';
      random = ''
        ruby -r securerandom -e "puts SecureRandom.hex(ARGV.first&.to_i || 32)" "''${@}"'';
      notmuch-ui = "emacs -nw -f notmuch";
      "mkinvoice" = ''mkdir "Invoices:$(date "+%y%m")"'';
    };
    shellGlobalAliases = let
      # expand ... to ../..; .... to ../../../ etc
      dots = (lib.lists.foldl' (acc: op:
        let
          name = "." + lib.concatStrings (lib.replicate op ".");
          cmd = lib.concatStrings (lib.replicate op "../");
        in acc // { "${name}" = cmd; }) { } (lib.range 2 4));
    in { } // dots;
    sessionVariables = { };
    autosuggestion = { enable = true; };
    historySubstringSearch = { enable = true; };
    initContent = lib.mkMerge [
      (lib.mkBefore ''
        export ZSH_AUTOSUGGEST_MANUAL_REBIND=false
        export NIXD_FLAGS="--inlay-hints=false"
        export FZF_CTRL_T_COMMAND="fd --type f --hidden --follow --exclude .git --exclude .devenv --exclude .direnv"
      '')
      (lib.mkAfter ''
        source ~/dotfiles/config/qq.zsh
        autoload -U compinit
        compinit -C # assume zcompdump is fresh

        bindkey "" history-beginning-search-forward
        bindkey "" history-beginning-search-backward
        bindkey '^W' backward-delete-word

        autoload -Uz add-zsh-hook
        # https://github.com/rothgar/mastering-zsh/blob/master/docs/config/hooks.md#add-function-to-hook
        do-ls() {${dir-summary}/bin/dir-summary $PWD;}

        # add do-ls to chpwd hook
        add-zsh-hook chpwd do-ls

        # TODO: fix on non-Darwin
        path+=("/opt/homebrew/bin/" "$HOME/dotfiles/bin" "$HOME/.emacs.doom/bin")
      '')
    ];
    oh-my-zsh = {
      enable = true;
      plugins = [ "tmux" "gpg-agent" "fancy-ctrl-z" "dircycle" "gitfast" ];
      extraConfig = ''
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
      {
        name = "fast-syntax-hightlighing";
        src = zsh-fast-syntax-highlighting;
        file =
          "share/zsh/plugins/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh";
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
        source <(${pkgs.just}/bin/just --completions zsh)
        compinit
      '';
    };
  in config.lib.dag.entryAfter [ "writeBoundary" ] "${script}";
}
