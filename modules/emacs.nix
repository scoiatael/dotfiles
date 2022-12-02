{ config, lib, pkgs, emacs-overlay, ... }:

let
  doomDir = builtins.fetchTarball {
    url =
      "https://github.com/doomemacs/doomemacs/archive/d5ccac5d71c819035fa251f01d023b3f94b4fba4.tar.gz";
    sha256 = "1hrhh3fa98nc9dc1a4x7slakmf3gfrqrcx4d4vg65rd8rb9wn37c";
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

  programs.emacs.enable = true;

  home.file.".config/doom".source = ../config/doom;
  home.file.".emacs.doom".source = doomDir;
  home.file.".emacs.d/early-init.el".text = ''
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "~/.emacs.local/")))
    (setenv "EMACSDIR" (expand-file-name (file-name-as-directory "~/.emacs.doom/")))
    (load (concat (expand-file-name (file-name-as-directory "${doomDir}")) "early-init.el") nil 'nomessage)
  '';
}
