{ config, lib, pkgs, agzam_spacehammer, AdamWagner_stackline, ... }:

{
  home.file.".hammerspoon" = let
    fennel = builtins.fetchTarball {
      url = "https://fennel-lang.org/downloads/fennel-1.4.2.tar.gz";
      sha256 = "sha256:1cvxpyr72wv42w5g1ghvp76rbxipmk3lh9yhy8j12fl4a8pf9cmi";
    };
  in {
    source = pkgs.symlinkJoin {
      name = "dot-hammerspoon";
      paths = (lib.sources.sourceFilesBySuffices fennel [ ".lua" ]);
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
  home.file.".hammerspoon/spacehammer".source = agzam_spacehammer;
  home.file.".hammerspoon/Spoons".source = ../config/Spoons;
  home.file.".hammerspoon/init.lua".source = ../config/hammerspoon.lua;
}

# Local Variables:
# compile-command: "home-manager switch --flake 'path:/Users/lucasczaplinski/dotfiles'"
# End:
