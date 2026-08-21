{ den, ... }:
{
  den.aspects.fish = {
    homeManager =
      {
        config,
        lib,
        pkgs,
        ...
      }:
      let
        inherit (lib) getExe optionalAttrs;
        nh = getExe pkgs.nh;
        date = "${pkgs.coreutils}/bin/date";
      in
      {
        programs.fish = {
          enable = true;

          shellAliases =
            {
              mv-desktop = "mv ~/Desktop/* '~/Google Drive/My Drive/Desktop/'";
              ee = "emacsclient --eval";
              kh = "edit ~/.ssh/known_hosts";
              gpg-wake = "echo 'foo' | gpg -e -r (gpg-fpr) --armour | gpg -d";
              xzf = "ouch decompress";
              lzf = "ouch list";
              pk = "pgr $argv | choose 1 | xargs kill";
              imgcat = "wezterm imgcat";
              da = "direnv allow";
              dr = "direnv reload";
              jl = "just --list --list-heading='' | fzf --reverse | choose 0 | xargs just";
              hmr = "${nh} home switch 'path:${config.home.homeDirectory}/dotfiles' -b bp.(${date} --iso-8601)";
              nix-test = "nix-build --keep-failed --expr 'with import <nixpkgs> {}; callPackage ./default.nix {}'";
              nix-test-python = "nix-build --keep-failed --expr 'let pkgs = import <nixpkgs> {}; in pkgs.python3Packages.callPackage ./default.nix {}'";
              nix-tree-devshell = "nix-tree --derivation '.#devShells.${pkgs.stdenv.hostPlatform.system}.default'";
              convert = "magick";
              g = "git";
              gb = "git checkout (g br --color=always | fzf --ansi | choose 0)";
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
              nix-dust = "nix path-info --size --recursive -h $DEVENV_PROFILE | sort -hk2";
              nix-du = "nix path-info -Sh $DEVENV_PROFILE";
              d = "direnv";
            }
            // (optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
              r = "trash";
              wssh = "wezterm cli spawn --domain-name SSH:$argv[1] && exit";
              os-switch = "${nh} darwin switch 'path:${config.home.homeDirectory}/dotfiles'";
            })
            // (optionalAttrs (!pkgs.stdenv.hostPlatform.isDarwin) {
              os-switch = "doas ${nh} os switch -R 'path:${config.home.homeDirectory}/dotfiles'";
            });

          functions = {
            pgr.body = "ps aux | grep $argv[1] | grep -v grep";
            gpg-fpr.body = "gpg -K --with-colons | grep fpr | head -n 1 | choose -f : -1";
            gpg-subkeys.body = ''
              gpg --list-secret-keys --with-subkey --with-colons | grep fpr | choose -f : -1 | grep -v (gpg-fpr) | xargs echo
            '';
            gpg-quick-expire-extend.body = ''
              gpg --quick-set-expire (gpg-fpr) 3m && gpg --quick-set-expire (gpg-fpr) 3m (gpg-subkeys)
            '';
            zsh_stats.body = "atuin history list --cmd-only | awk '{CMD[$1]++;count++;}END { for (a in CMD)print CMD[a] \" \" CMD[a]/count*100 \"% \" a;}' | grep -v './' | column -c3 -s ' ' -t | sort -nr | nl | head -n10";
            random.body = ''ruby -r securerandom -e "puts SecureRandom.hex(ARGV.first&.to_i || 32)" $argv'';
            poi.body = "gh poi --dry-run; and read --prompt 'ok? [Y/y] ' ok; and string match -riq '^y' $ok; and gh poi; or echo 'aborted by prompt'";
            notmuch-ui.body = "emacs -nw -f notmuch";

            # run dir-summary when changing directories
            __fish_cd_hook = {
              onVariable = "PWD";
              body = "dir-summary $PWD";
            };
          };

          interactiveShellInit = ''
            fish_add_path /opt/homebrew/bin "$HOME/dotfiles/bin" "$HOME/.emacs.doom/bin"

            if command -q wt
              wt config shell init fish | source
            end

            set -gx LESSOPEN "|${pkgs.lesspipe}/bin/lesspipe.sh %s"
            set -gx LESS "-R"
            set -gx NIXD_FLAGS "--inlay-hints=false"
            set -gx FZF_CTRL_T_COMMAND "fd --type f --hidden --follow --exclude .git --exclude .devenv --exclude .direnv"
            set -gx DOOMLOCALDIR "~/.emacs.local"
            set -gx EMACSDIR "~/.emacs.doom"
          '';

          plugins = [
            {
              name = "tide";
              src = pkgs.fishPlugins.tide.src;
            }
          ];
        };
      };
  };
}
