{ config, lib, pkgs, agzam_spacehammer, AdamWagner_stackline, ... }:

{
  home.file.".hammerspoon" = let
    fennel = builtins.fetchTarball {
      url = "https://fennel-lang.org/downloads/fennel-1.4.2.tar.gz";
      sha256 = "sha256:1cvxpyr72wv42w5g1ghvp76rbxipmk3lh9yhy8j12fl4a8pf9cmi";
    };
    spoons = pkgs.stdenv.mkDerivation {
      name = "dotfiles-spoons";
      dontUnpack = true;
      installPhase = ''
        mkdir -p $out/
        cp -rv ${../config/Spoons} $out/Spoons
      '';
    };
    # TODO: migrate to Fennel
    my-lua-config = pkgs.stdenv.mkDerivation {
      name = "dotfiles-my-lua-config";
      dontUnpack = true;
      installPhase = ''
        mkdir -p $out
        cp -rv ${../config/hammerspoon.lua} $out/my-lua-config.lua
      '';
    };
  in {
    source = pkgs.symlinkJoin {
      name = "dot-hammerspoon";
      paths = [
        (lib.sources.sourceFilesBySuffices agzam_spacehammer [
          ".lua"
          ".fnl"
          ".el"
        ])
        (lib.sources.sourceFilesBySuffices fennel [ ".lua" ])
        spoons
        my-lua-config
      ];
    };
    target = ".hammerspoon";
    recursive = true;
  };

  # https://fennel-lang.org/reference
  home.file.".spacehammer" = {
    source = ../config/spacehammer;
    recursive = true;
  };

  home.file.".hammerspoon/stackline".source = AdamWagner_stackline;
}

# Local Variables:
# compile-command: "home-manager switch --flake 'path:/Users/lucasczaplinski/dotfiles'"
# End:
