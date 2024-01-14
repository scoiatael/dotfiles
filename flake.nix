{
  description = "Home-manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:azuwis/nix-darwin/sketchybar";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware";
    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    doomemacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };
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
          ./modules/sway.nix
          ({ lib, ... }: {
            programs.alacritty.settings.font.size = lib.mkForce 10;
          })
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
          system = "aarch64-darwin";
          config.allowBroken = true;
        };
        modules = [
          ./modules/home.nix
          ./modules/git.nix
          ./modules/emacs.nix
          ./modules/tmux.nix
          ./modules/zsh.nix
          ./modules/neovim.nix
          ({ pkgs, ... }: { programs.emacs.package = pkgs.emacs29-macport; })
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
    homeConfigurations."lukaszczaplinski@LsAir" =
      home-manager.lib.homeManagerConfiguration {
        pkgs = import nixpkgs {
          system = "aarch64-darwin";
          config.allowBroken = true;
        };
        modules = [
          ./modules/home.nix
          ./modules/git.nix
          ./modules/emacs.nix
          ./modules/tmux.nix
          ./modules/zsh.nix
          ./modules/neovim.nix
          ({ pkgs, ... }: {
            programs.emacs.package =
              # https://bitbucket.org/mituharu/emacs-mac/commits/5f6c306095c825eb01708e336f9d03c15271dfe9
              # see https://github.com/doomemacs/doomemacs/issues/7532
              pkgs.emacs29-macport.overrideAttrs {
                src = pkgs.fetchFromBitbucket {
                  owner = "mituharu";
                  repo = "emacs-mac";
                  rev = "5f6c306095c825eb01708e336f9d03c15271dfe9";
                  hash = "sha256-0OHa9znJzJ1g4mE2vOwt7C/ER40Uh1Y3hgnE+m4g1tw=";
                };
              };
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
        ./modules/darwin/ollama.nix
        ./modules/darwin/shortcat.nix
      ];
    };
    darwinConfigurations.LsAir = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./modules/darwin.nix
        ./modules/darwin/yabai.nix
        ./modules/darwin/sketchybar.nix
        ./modules/darwin/air.nix
        ./modules/darwin/shortcat.nix
        ./modules/darwin/ollama.nix
      ];
    };
    nixosConfigurations.LsFramework = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      specialArgs = attrs;
      modules = [ ./modules/nixos.nix ./modules/nixos/smb.nix ];
    };
  };
}
