{ doomemacs, ... }:
{ config, lib, pkgs, ... }:
let emacsPackage = config.programs.emacs.finalPackage;
in {
  home.packages = with pkgs; [
    #recutils
    aspell
    aspellDicts.pl
    aspellDicts.cs
    aspellDicts.en
    aspellDicts.en-computers
    enchant
    nixfmt-classic
    sqlite
    clang
    hunspell
    hunspellDicts.pl_PL
    hunspellDicts.en-gb-ise
    # TODO: https://github.com/aca/emmet-ls
    zstd.bin
    pass

    vtsls # LSP for JS
    rufo # formatter for Ruby

    nerd-fonts.victor-mono
    nerd-fonts.fira-code
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.vterm
      epkgs.jinx
      epkgs.fennel-mode
      epkgs.flymake-fennel
    ];
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
    (load (concat (expand-file-name (file-name-as-directory "${doomemacs}")) "early-init.el") nil 'nomessage)
  '';

  xdg.configFile."enchant/enchant.ordering".text = ''
    *:aspell,hunspell,nuspell
  '';
}
