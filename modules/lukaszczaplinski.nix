{
  den,
  ...
}:
{
  den.aspects.lukaszczaplinski = {
    includes = [
      den.batteries.define-user
      den.batteries.primary-user
    ];
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
