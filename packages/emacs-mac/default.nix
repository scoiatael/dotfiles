{ pkgs ? import <nixpkgs> {}, ... }:
pkgs.emacs29.overrideAttrs (finalAttrs: previousAttrs: {
  # https://github.com/d12frosted/homebrew-emacs-plus/tree/master/patches/emacs-29
  patches = previousAttrs.patches ++ [
    ./patches/fix-window-role.patch
    ./patches/no-frame-refocus-cocoa.patch
    ./patches/round-undecorated-frame.patch
    ./patches/system-appearance.patch
  ];
})
