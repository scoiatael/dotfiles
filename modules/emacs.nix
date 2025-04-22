{doomemacs, ...}: {
  config,
  lib,
  pkgs,
  ...
}: let
  emacsPackage = config.programs.emacs.finalPackage;
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

      victor-mono
      nerd-fonts.symbols-only

      # notmuch # mail

      nuspell # spell checking

      # for emacsclient.sh
      gnused
      coreutils-full
    ]
    ++ (with pkgs.hunspellDicts; [pl_PL en_GB-ise]);

  fonts.fontconfig.enable = true; # required to autoload fonts from packages

  # programs.afew.enable = true;

  programs.zsh.sessionVariables.NOTMUCH_CONFIG = "${config.home.homeDirectory}/Mail/notmuch-config";

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [epkgs.vterm epkgs.jinx];
  };
  programs.zsh.sessionVariables = {
    DOOMLOCALDIR = "$HOME/.emacs.local";
    LSP_USE_PLISTS = "true";
    EDITOR = "env PATH=${config.home.homeDirectory}/.nix-profile/bin ${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/emacs/emacsclient.sh";
  };

  # avoid copying into /nix/store to allow easy changes
  # home.file.".config/doom".source = config.lib.file.mkOutOfStoreSymlink "~/dotfiles/config/doom";
  home.activation.linkDoomConfig = config.lib.dag.entryAfter ["writeBoundary"] ''
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
}
