{ config, lib, pkgs, doomemacs, ... }:

let emacsPackage = config.programs.emacs.package; in
{
  home.packages = with pkgs; [
    recutils
    aspell
    aspellDicts.pl
    aspellDicts.cs
    aspellDicts.en
    aspellDicts.en-science
    aspellDicts.en-computers
    enchant
    nixfmt
    sqlite
    clang
    # TODO: https://github.com/aca/emmet-ls
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm epkgs.jinx ];
  };

  # avoid copying into /nix/store to allow easy changes
  # home.file.".config/doom".source = config.lib.file.mkOutOfStoreSymlink "~/dotfiles/config/doom";
  home.activation.linkDoomConfig =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      test -d ~/.config || mkdir ~/.config
      test -d ~/.config/doom || ln -sf ~/dotfiles/config/doom ~/.config/doom
    '';

  home.file.".emacs.doom".source = doomemacs;
  home.file.".emacs.d/early-init.el".text = ''
    (setq envrc-direnv-executable "${pkgs.direnv}/bin/direnv")
    (load "${emacsPackage}/share/emacs/site-lisp/site-start" nil 'nomessage)
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "~/.emacs.local/")))
    (setenv "EMACSDIR" (expand-file-name (file-name-as-directory "~/.emacs.doom/")))
    (load (concat (expand-file-name (file-name-as-directory "${doomemacs}")) "early-init.el") nil 'nomessage)
  '';

  xdg.configFile.".enchant/enchant.ordering".text = ''
    *:aspell,hunspell,nuspell
  '';
}
