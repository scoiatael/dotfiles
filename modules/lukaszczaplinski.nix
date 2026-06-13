{
  den,
  ...
}:
{
  den.aspects.lukaszczaplinski = {
    includes = [
      den.batteries.define-user
      den.batteries.primary-user
      den.aspects.stylix
    ];
    darwin = {
      home-manager.backupFileExtension = "hm.bak";
    };
    nixos = {
      home-manager.backupFileExtension = "hm.bak";
    };
    homeManager = {
      imports = [
        ../home/modules/default.nix
        ../home/modules/git.nix
        ../home/modules/editors/neovim.nix
        ../home/modules/cli.nix
        ../home/modules/multiplexers/tmux.nix
        ../home/modules/shells/zsh.nix
      ];
    };
  };
}
