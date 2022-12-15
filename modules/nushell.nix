{ config, lib, pkgs, ... }:

let
  macConfigPath = "Library/Application Support/nushell";
  ssh_auth_sock = if pkgs.stdenv.isDarwin then
    "${config.home.homeDirectory}/.gnupg/S.gpg-agent.ssh"
  else
    "..."; # TODO
in {
  programs.nushell = {
    enable = true;
    configFile = {
      text = ''
        let-env config = {
          filesize_metric: false
          table_mode: rounded
          use_ls_colors: true
          show_banner: false
        }
        # https://raw.githubusercontent.com/nushell/nu_scripts/main/direnv/config.nu
        let-env config = ($env.config | upsert hooks {
          pre_prompt: { code: 'load-env (let direnv = (~/.nix-profile/bin/direnv export json | from json); if ($direnv | length) > 0 { $direnv } else { {} })' }
        })
        alias hmr = home-manager switch --flake "path:${config.home.homeDirectory}/dotfiles"
        alias l = ^exa
        alias ll = l -l

        alias nix-test = nix-build --keep-failed --expr 'with import <nixpkgs> {}; callPackage ./default.nix {}'

        source ~/dotfiles/config/nushell/starship.nu
        source ~/dotfiles/config/nushell/zoxide.nu
        source ~/dotfiles/config/nushell/vterm.nu
      '';
    };
    envFile = {
      text = ''
        let-env SSH_AUTH_SOCK =  "${ssh_auth_sock}"
        let-env ENV_CONVERSIONS = {
          "PATH": {
            from_string: { |s| $s | split row (char esep) }
            to_string: { |v| $v | str collect (char esep) }
          }
          "Path": {
            from_string: { |s| $s | split row (char esep) }
            to_string: { |v| $v | str collect (char esep) }
          }
        }
        let-env PATH = ($env.PATH
          | append '/usr/local/bin'
          | append '${config.home.homeDirectory}/.emacs.doom/bin'
          | append '${config.home.homeDirectory}/.emacs.d/bin'
          | append '${config.home.homeDirectory}/dotfiles/bin')
      '';
    };
  };

  home.file = lib.optionalAttrs pkgs.stdenv.isDarwin {
    "${macConfigPath}/config.nu".text = ''
      source  ~/.config/nushell/config.nu
    '';
    "${macConfigPath}/env.nu".text = ''
      source  ~/.config/nushell/env.nu
    '';
  };
}
