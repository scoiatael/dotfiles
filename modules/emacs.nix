{ config, lib, pkgs, emacs-overlay, ... }:

{
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

  home.file.".config/doom".source = ~/dotfiles/config/doom;
  home.file.".emacs.doom".source =
    builtins.fetchGit { url = "https://github.com/doomemacs/doomemacs.git"; };
  home.file.".emacs.d/early-init.el".text = ''
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "~/.emacs.local/")))
    (setenv "LSP_USE_PLISTS" "true")
    (setq user-emacs-directory (expand-file-name (file-name-as-directory "~/.emacs.doom/")))
    (load (concat user-emacs-directory "early-init.el") nil 'nomessage)
  '';

}
