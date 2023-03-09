{ config, lib, pkgs, emacs-overlay, ... }:

let
  doomDir = builtins.fetchTarball {
    url =
      "https://github.com/doomemacs/doomemacs/archive/8f6b045dfdb6d00e5a324c3f9044dce773791502.tar.gz";
    sha256 = "1fzgwxvj60dfjdgylibjbg407d1lb35269ykgy7jcjxdmmbfzl9b";
  };
in {
  nixpkgs.overlays = [ emacs-overlay.overlay ];

  home.packages = with pkgs; [
    recutils
    aspell
    aspellDicts.pl
    aspellDicts.cs
    aspellDicts.en
    aspellDicts.en-science
    aspellDicts.en-computers
    ispell
    nixfmt
    sqlite
    clang
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [ epkgs.vterm ];
  };

  # avoid copying into /nix/store to allow easy changes
  # home.file.".config/doom".source = config.lib.file.mkOutOfStoreSymlink "~/dotfiles/config/doom";
  home.activation.linkDoomConfig =
    config.lib.dag.entryAfter [ "writeBoundary" ] ''
      test -d ~/.config/doom || ln -sf ~/dotfiles/config/doom ~/.config/doom
    '';

  home.file.".emacs.doom".source = doomDir;
  home.file.".emacs.d/early-init.el".text = ''
    (setq envrc-direnv-executable "${pkgs.direnv}/bin/direnv")
    (load "${config.home.homeDirectory}/.nix-profile/share/emacs/site-lisp/site-start.el" nil 'nomessage)
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "~/.emacs.local/")))
    (setenv "EMACSDIR" (expand-file-name (file-name-as-directory "~/.emacs.doom/")))
    (load (concat (expand-file-name (file-name-as-directory "${doomDir}")) "early-init.el") nil 'nomessage)
  '';
}
