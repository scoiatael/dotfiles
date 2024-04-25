{ config, lib, pkgs, ... }:

{
  home.file.".hammerspoon" = let
    spacehammer = pkgs.fetchFromGitHub {
      owner = "agzam";
      repo = "spacehammer";
      rev = "bfbe3de";
      sha256 = "sha256-UD0NF30eQZ+6UzoQ3B3eoaUrY9DxoYtQ1RZafHIh/aQ=";
    };
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
        (lib.sources.sourceFilesBySuffices spacehammer [ ".lua" ".fnl" ".el" ])
        (lib.sources.sourceFilesBySuffices fennel [ ".lua" ])
        spoons
        my-lua-config
      ];
    };
    target = ".hammerspoon";
    recursive = true;
  };

  # https://fennel-lang.org/reference
  home.file."/.spacehammer" = {
    source = ../config/spacehammer;
    recursive = true;
  };
}

# Local Variables:
# compile-command: "home-manager switch --flake 'path:/Users/lucasczaplinski/dotfiles'"
# End:
