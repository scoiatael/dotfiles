{ config, lib, pkgs, ... }:

let
  doomDir = builtins.fetchTarball {
    url =
      "https://github.com/doomemacs/doomemacs/archive/042fe0c43831c8575abfdec4196ebd7305fa16ac.tar.gz";
    sha256 = "1z9cjksph1q0v6sz1lhnvdcv1hd5cx7vyzi3fn28ph7q0yxmq62y";
  };
in {
  home.packages = with pkgs; [
    recutils
    aspell
    aspellDicts.pl
    aspellDicts.cs
    aspellDicts.en
    aspellDicts.en-science
    aspellDicts.en-computers
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

  home.file.".emacs.doom".source = doomDir;
  home.file.".emacs.d/early-init.el".text = ''
    (setq envrc-direnv-executable "${pkgs.direnv}/bin/direnv")
    (load "${config.home.homeDirectory}/.nix-profile/share/emacs/site-lisp/site-start.el" nil 'nomessage)
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "~/.emacs.local/")))
    (setenv "EMACSDIR" (expand-file-name (file-name-as-directory "~/.emacs.doom/")))
    (load (concat (expand-file-name (file-name-as-directory "${doomDir}")) "early-init.el") nil 'nomessage)
  '';
}
