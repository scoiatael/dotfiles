{
  den,
  dotfiles,
  ...
}:
{
  den.aspects.lukaszczaplinski = {
    includes = [
      den.batteries.define-user
      den.batteries.primary-user
      den.aspects.stylix
      den.aspects.neovim
      (den.batteries.user-shell "zsh")
      den.batteries.mkBackupCommand
    ];
    homeManager = {
      imports = [
        dotfiles.homeModules.zsh
        ../_home/modules/default.nix
        ../_home/modules/git.nix
        ../_home/modules/cli.nix
        ../_home/modules/multiplexers/tmux.nix
      ];
    };
  };
}
