{ config, lib, pkgs, ... }:

let
  macConfigPath = "Library/Application Support/nushell";
  system = if pkgs.stdenv.isDarwin then "macos" else "framework";
  ssh_auth_sock = if pkgs.stdenv.isDarwin then "${config.home.homeDirectory}/.gnupg/S.gpg-agent.ssh" else "..."; # TODO
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
        alias hmr = home-manager switch --flake "path:${config.home.homeDirectory}/dotfiles#${system}"
        alias l = ^exa
        alias ll = l -l

        alias nix-test = nix-build --keep-failed --expr 'with import <nixpkgs> {}; callPackage ./default.nix {}'

        source ~/dotfiles/config/nushell/starship.nu
        source ~/dotfiles/config/nushell/zoxide.nu
      '';
    };
    envFile = {
      text = ''
        let-env SSH_AUTH_SOCK =  "${ssh_auth_sock}" 
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
