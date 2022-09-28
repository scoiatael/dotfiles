{ config, lib, pkgs, emacs-overlay, ... }:

let
  doomDir = builtins.fetchTarball {
    url =
      "https://github.com/doomemacs/doomemacs/archive/5a5195b84d2fade4de1d55ec5441277a0e2cc591.tar.gz";
    sha256 = "0hajb96qx6hyavmaa8p41ml83waqmrwaph2aa8akyrgpapglf464";
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
    (setenv "LSP_USE_PLISTS" "true")
    (setq user-emacs-directory (expand-file-name (file-name-as-directory "${doomDir}")))
    (load (concat user-emacs-directory "early-init.el") nil 'nomessage)
  '';

}
