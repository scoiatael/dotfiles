{
  description = "Home-manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    persway.url =
      "github:scoiatael/persway/f54ef7dbe5f04b43c901f767eb5230db98b9e0ed";
    macNixpkgs.url = "github:nixos/nixpkgs/nixpkgs-22.05-darwin";
    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "macNixpkgs";
  };

  outputs = { nixpkgs, home-manager, darwin, ... }@attrs: {
    homeConfigurations.framework = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        ./modules/home.nix
        ./modules/electron.nix
        ./modules/linux.nix
        ./modules/rclone-gdrive-mount.nix
        ./modules/sway.nix
        ./modules/git.nix
        ./modules/emacs.nix
        ./modules/tmux.nix
        ./modules/newm.nix
        ({ pkgs, ... }: {
          programs.emacs.package = pkgs.emacsPgtk.override { };
        })
        {
          home = {
            username = "lczaplinski";
            homeDirectory = "/home/lczaplinski";
            stateVersion = "21.11";
          };
          programs.home-manager.enable = true;
        }
      ];
      extraSpecialArgs = attrs;
    };
    homeConfigurations.lukaszczaplinski =
      home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-darwin;
        modules = [
          ./modules/home.nix
          ./modules/git.nix
          ./modules/emacs.nix
          ./modules/tmux.nix
          ./modules/nushell.nix
          ({ pkgs, ... }: {
            # programs.emacs.package = pkgs.callPackage ./packages/emacs-mac { };
            programs.emacs.package = pkgs.emacsMacport;
          })
          {
            home = {
              username = "lukaszczaplinski";
              homeDirectory = "/Users/lukaszczaplinski";
              stateVersion = "22.05";
            };
            programs.home-manager.enable = true;
          }
        ];
        extraSpecialArgs = attrs;
      };
    darwinConfigurations.LsMBP = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      modules = [ ./modules/darwin.nix ];
    };
  };
}
