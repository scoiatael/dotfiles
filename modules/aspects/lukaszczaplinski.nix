{
  den,
  dotfiles,
  ...
}:
let
  mkBackupCommand =
    pkgs:
    let
      date = "${pkgs.coreutils}/bin/date";
    in
    pkgs.writeShellScriptBin "mv-backup" ''
      mv "$1" "$1"."$(${date} --iso-8601=s)".bak
    '';
in
{
  den.aspects.lukaszczaplinski = {
    includes = [
      den.batteries.define-user
      den.batteries.primary-user
      den.aspects.stylix
      den.aspects.neovim
      (den.batteries.user-shell "zsh")
    ];
    darwin = { pkgs, ... }: {
      home-manager.backupCommand = mkBackupCommand pkgs;
    };
    nixos = { pkgs, ... }: {
      home-manager.backupCommand = mkBackupCommand pkgs;
    };
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
