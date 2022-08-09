{ config, lib, pkgs, emacs-overlay, ... }:

let
  doomDir = builtins.fetchGit {
    url = "https://github.com/doomemacs/doomemacs.git";
    rev = "b06fd63dcb686045d0c105f93e07f80cb8de6800";
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
    package = pkgs.emacsPgtkNativeComp;
  };

  home.file.".config/doom".source = ../config/doom;
  home.file.".emacs.d/early-init.el".text = ''
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "~/.emacs.local/")))
    (setenv "LSP_USE_PLISTS" "true")
    (setq user-emacs-directory (expand-file-name (file-name-as-directory "${doomDir}")))
    (load (concat user-emacs-directory "early-init.el") nil 'nomessage)
  '';

}
