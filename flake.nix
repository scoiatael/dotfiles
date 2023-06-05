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
    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # TODO:
    # doomemacs = {
    #   url = "github:doomemacs/doomemacs";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };
  };

  outputs = { nixpkgs, home-manager, darwin, ... }@attrs: {
    homeConfigurations."lukaszczaplinski@LsFramework" =
      home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [
          ./modules/home.nix
          ./modules/electron.nix
          ./modules/linux.nix
          ./modules/git.nix
          ./modules/emacs.nix
          ./modules/tmux.nix
          ./modules/zsh.nix
          ./modules/neovim.nix
          ({ lib, ... }: {
            programs.alacritty.settings.font.size = lib.mkForce 10;
          })
          ({ pkgs, ... }: { programs.emacs.package = pkgs.emacsPgtk; })
          {
            home = {
              username = "lukaszczaplinski";
              homeDirectory = "/home/lukaszczaplinski";
              stateVersion = "22.11";
            };
            programs.home-manager.enable = true;
          }
        ];
        extraSpecialArgs = attrs;
      };
    homeConfigurations."lucasczaplinski@LsCerosDarwin" =
      home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          system = "x86_64-darwin";
          config.allowBroken = true;
        };
        modules = [
          ./modules/home.nix
          ./modules/git.nix
          ./modules/emacs.nix
          ./modules/tmux.nix
          ./modules/zsh.nix
          ./modules/neovim.nix
          ({ pkgs, ... }: { programs.emacs.package = pkgs.emacsMacport; })
          ({ pkgs, ... }: { home.packages = with pkgs; [ lastpass-cli ]; })
          {
            home = {
              username = "lucasczaplinski";
              homeDirectory = "/Users/lucasczaplinski";
              stateVersion = "22.05";
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
          ./modules/neovim.nix
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
      modules = [
        ./modules/darwin.nix
        ./modules/darwin/yabai.nix
        ./modules/darwin/sketchybar.nix
        ./modules/darwin/gaming.nix
      ];
    };
    darwinConfigurations.LsCerosDarwin = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./modules/darwin.nix
        ./modules/darwin/yabai.nix
        ./modules/darwin/sketchybar.nix
        ./modules/darwin/ceros.nix
      ];
    };
    nixosConfigurations.LsFramework = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [ ./modules/nixos.nix ];
    };
  };
}
