{
  description = "Home-manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:LnL7/nix-darwin";
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
    emacsMacport = {
      url = "git+https://bitbucket.com/mituharu/emacs-mac?ref=work";
      flake = false;
    };
    gitAlias = {
      url = "github:GitAlias/gitalias";
      flake = false;
    };
    # catppuccin-rio = {
    #   url = "github:catppuccin/rio";
    #   flake = false;
    # };
    # rio = {
    #   url = "github:raphamorim/rio/0.0.x";
    #   flake = false;
    # };
    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    talonhub_community = {
      url = "github:talonhub/community";
      flake = false;
    };
    cursorless_talon = {
      url = "github:cursorless-dev/cursorless-talon";
      flake = false;
    };
    agzam_spacehammer = {
      url = "github:agzam/spacehammer";
      flake = false;
    };
    AdamWagner_stackline = {
      url = "github:AdamWagner/stackline";
      flake = false;
    };
  };

  outputs = { nixpkgs, home-manager, darwin, emacsMacport, ... }@attrs:
    let
      patchedEmacsMacport = { pkgs, ... }: {
        programs.emacs.package =
          # https://bitbucket.org/mituharu/emacs-mac/commits/5f6c306095c825eb01708e336f9d03c15271dfe9
          # see https://github.com/doomemacs/doomemacs/issues/7532
          pkgs.emacs29-macport.overrideAttrs { src = emacsMacport; };
      };
      lix = { pkgs, ... }: { nix.package = pkgs.lix; };
    in {
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
      homeConfigurations."wooting@LsWootingMBP" =
        home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "x86_64-darwin";
            config.allowBroken = true;
          };
          modules = [
            ./modules/home.nix
            ./modules/hammerspoon-config.nix
            ./modules/git.nix
            ./modules/emacs.nix
            ./modules/tmux.nix
            ./modules/zsh.nix
            ./modules/neovim.nix
            patchedEmacsMacport
            {
              programs.git.extraConfig.user = {
                email = "lukasz@wooting.io";
                name = "Lukas Czaplinski";
                signingkey = "E871295C0EFA7DBFA9E673CC7135745D2C62273D";
              };
            }
            {
              home = {
                username = "wooting";
                homeDirectory = "/Users/wooting";
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
            #./modules/cursorless.nix # requires talon in nix-darwin
            ./modules/home.nix
            ./modules/wezterm.nix
            ./modules/git.nix
            ./modules/hammerspoon-config.nix
            ./modules/emacs.nix
            ./modules/tmux.nix
            ./modules/zsh.nix
            ./modules/neovim.nix
            patchedEmacsMacport
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
      darwinConfigurations.LsWootingMBP = darwin.lib.darwinSystem {
        system = "x86_64-darwin";
        modules = [
          lix
          ./modules/darwin.nix
          ./modules/darwin/yabai.nix
          ./modules/darwin/wooting.nix
          ./modules/darwin/sketchybar.nix
          ./modules/darwin/hammerspoon.nix
        ];
      };
      darwinConfigurations.LsAir = darwin.lib.darwinSystem {
        system = "aarch64-darwin";
        modules = [
          lix
          ./modules/darwin.nix
          ./modules/darwin/yabai.nix
          ./modules/darwin/talon.nix
          ./modules/darwin/sketchybar.nix
          ./modules/darwin/air.nix
          ./modules/darwin/shortcat.nix
          ./modules/darwin/ollama.nix
          ./modules/darwin/hammerspoon.nix
        ];
      };
      nixosConfigurations.LsFramework = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = attrs;
        modules = [
          ./modules/nixos.nix
          ./modules/nixos/smb.nix
          ./modules/nixos/jellyfin.nix
          ./modules/nixos/steam.nix
        ];
      };
    };
}
