# DO-NOT-EDIT. This file was auto-generated using github:vic/flake-file.
# Use `nix run .#write-flake` to regenerate it.
{
  description = "My den-ful nix setup (home-manager, nixos, nix-darwin)";

  outputs = inputs: inputs.flake-parts.lib.mkFlake { inherit inputs; } (inputs.import-tree ./modules);

  inputs = {
    darwin.url = "github:LnL7/nix-darwin/nix-darwin-26.05";
    den.url = "github:denful/den";
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
    flake-file.url = "github:vic/flake-file";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    home-manager.url = "github:nix-community/home-manager/release-26.05";
    import-tree.url = "github:denful/import-tree";
    lanzaboote.url = "github:nix-community/lanzaboote";
    llm-agents.url = "github:numtide/llm-agents.nix";
    niri-nix.url = "git+https://codeberg.org/BANanaD3V/niri-nix";
    nix-auto-follow = {
      url = "github:fzakaria/nix-auto-follow";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nix-index-database.url = "github:nix-community/nix-index-database";
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    nixpkgs.url = "github:nixos/nixpkgs/nixos-26.05";
    noctalia.url = "github:noctalia-dev/noctalia";
    parrhasius = {
      url = "git+https://git.sr.ht/~scoiatael/parrhasius";
      flake = false;
    };
    sops-nix.url = "github:Mic92/sops-nix";
    stylix.url = "github:nix-community/stylix/release-26.05";
    walker.url = "github:abenz1267/walker";
  };
}
