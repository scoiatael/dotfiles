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
    darwin = {
      url = "github:azuwis/nix-darwin/sketchybar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    grub2-themes = {
      url = "github:vinceliuice/grub2-themes";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, home-manager, darwin, ... }@attrs: {
    homeConfigurations.lczaplinski = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      modules = [
        ./modules/home.nix
        ./modules/electron.nix
        ./modules/linux.nix
        ./modules/rclone-gdrive-mount.nix
        ./modules/git.nix
        ./modules/emacs.nix
        ./modules/tmux.nix
        ./modules/zsh.nix
        ({ pkgs, ... }: { programs.emacs.package = pkgs.emacsGit; })
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
    homeConfigurations."lukaszczaplinski@LsGamingDarwin" =
      home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-darwin;
        modules = [
          ./modules/home.nix
          ./modules/git.nix
          ./modules/emacs.nix
          ./modules/tmux.nix
          ./modules/zsh.nix
          ({ pkgs, ... }: { programs.emacs.package = pkgs.emacsMacport; })
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
    darwinConfigurations.LsGamingDarwin = darwin.lib.darwinSystem {
      system = "x86_64-darwin";
      modules = [ ./modules/darwin.nix ];
    };
    nixosConfigurations.r-work-nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [ ./modules/nixos.nix ];
    };
  };
}
