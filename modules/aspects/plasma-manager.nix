{ inputs, ... }:
{
  flake-file.inputs.plasma-manager = {
    url = "github:nix-community/plasma-manager";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.home-manager.follows = "home-manager";
  };

  den.aspects.plasma-manager.homeManager = {
    imports = [ inputs.plasma-manager.homeModules.plasma-manager ];
    programs.plasma.enable = true;
  };
}
