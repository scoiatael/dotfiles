{
  imports = [ ../../.nix/unfree.nix ];
  allowUnfreePackages = [
    "steam"
    "steam-original"
    "steam-run"
    "steam-unwrapped"
  ];
  programs.steam = {
    enable = true;
  };
}
