{
  description = "Home-manager configuration";

  inputs.nix-index-database.url = "github:nix-community/nix-index-database";
  inputs.nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
  inputs.just-flake.url = "github:wootingkb/just-flake";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.home-manager = {
    url = "github:nix-community/home-manager";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.darwin = {
    url = "github:LnL7/nix-darwin";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware";
  inputs.lanzaboote = {
    url = "github:nix-community/lanzaboote";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.walker.url = "github:abenz1267/walker";
  inputs.fastanime = {
    url = "github:Benexl/FastAnime";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.doomemacs = {
    url = "github:doomemacs/doomemacs";
    flake = false;
  };
  inputs.gitAlias = {
    url = "github:GitAlias/gitalias";
    flake = false;
  };
  inputs.nixSearch = {
    url = "github:diamondburned/nix-search";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.lix = {
    url = "https://git.lix.systems/lix-project/lix/archive/main.tar.gz";
    flake = false;
  };
  inputs.lix-module = {
    url =
      "https://git.lix.systems/lix-project/nixos-module/archive/main.tar.gz";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.lix.follows = "lix";
  };
  # inputs.catppuccin-rio = {
  #   url = "github:catppuccin/rio";
  #   flake = false;
  # };
  # inputs.rio = {
  #   url = "github:raphamorim/rio/0.0.x";
  #   flake = false;
  # };
  # inputs.nix-vscode-extensions = {
  #   url = "github:nix-community/nix-vscode-extensions";
  #   inputs.nixpkgs.follows = "nixpkgs";
  #   inputs.flake-utils.follows = "flake-utils";
  # };
  # inputs.talonhub_community = {
  #   url = "github:talonhub/community";
  #   flake = false;
  # };
  # inputs.cursorless_talon = {
  #   url = "github:cursorless-dev/cursorless-talon";
  #   flake = false;
  # };
  # inputs.agzam_spacehammer = {
  #   url = "github:agzam/spacehammer";
  #   flake = false;
  # };
  # inputs.AdamWagner_stackline = {
  #   url = "github:AdamWagner/stackline";
  #   flake = false;
  # };

  outputs = { self, flake-parts, nixpkgs, home-manager, darwin, ... }@attrs:
    flake-parts.lib.mkFlake { inputs = attrs; }
    (top@{ config, withSystem, moduleWithSystem, ... }: {
      imports = [ attrs.just-flake.flakeModule ];
      flake =
        let lix = { pkgs, lib, ... }: { nix.package = lib.mkForce pkgs.lix; };
        in {
          homeManagerModules = {
            wooting = {
              imports = [
                ./modules/home.nix
                ./modules/cli.nix
                (import ./modules/home-manager.nix attrs)
                ./modules/git.nix
                ./modules/tmux.nix
                (import ./modules/zsh.nix attrs)
                (import ./modules/emacs.nix attrs)
                ./modules/wezterm.nix
                ./modules/llm.nix
                ./modules/neovim.nix
                (import ./modules/nix-search.nix attrs)
                ({ pkgs, ... }: {
                  programs.emacs.package = pkgs.emacs-macport.override {
                    withNativeCompilation = false;
                  };
                })
                {
                  programs.git.extraConfig.user = {
                    email = "lukasz@wooting.io";
                    name = "Lukas Czaplinski";
                    signingkey = "E871295C0EFA7DBFA9E673CC7135745D2C62273D";
                  };
                }
                {
                  home = {
                    username = "lukas";
                    homeDirectory = "/Users/lukas";
                    stateVersion = "22.05";
                  };
                }
              ];
            };
          };
          homeConfigurations = {
            "l@LsNixOS" = home-manager.lib.homeManagerConfiguration {
              pkgs = nixpkgs.legacyPackages.x86_64-linux;
              modules = [
                attrs.walker.homeManagerModules.default
                ./modules/walker.nix
                ./modules/home.nix
                ./modules/ghostty.nix
                ./modules/electron.nix
                ./modules/linux.nix
                ./modules/cli.nix
                ./modules/git.nix
                (import ./modules/zsh.nix attrs)
                ./modules/neovim.nix
                ./modules/wezterm.nix
                (import ./modules/home-manager.nix attrs)
                (import ./modules/emacs.nix attrs)
                {
                  programs.git.extraConfig.user = {
                    email = "lukasz@wooting.io";
                    name = "Lukas Czaplinski";
                    signingkey = "E871295C0EFA7DBFA9E673CC7135745D2C62273D";
                  };
                }
                {
                  home = {
                    username = "l";
                    homeDirectory = "/home/l";
                    stateVersion = "24.11";
                  };
                }
              ];
            };
            "lukaszczaplinski@LsFramework" =
              home-manager.lib.homeManagerConfiguration {
                pkgs = nixpkgs.legacyPackages.x86_64-linux;
                modules = [
                  ./modules/home.nix
                  ./modules/zellij.nix
                  ./modules/cli.nix
                  ./modules/linux.nix
                  ./modules/git.nix
                  ./modules/tmux.nix
                  (import ./modules/zsh.nix attrs)
                  ./modules/neovim.nix
                  ({ pkgs, lib, ... }: {
                    programs.zsh.sessionVariables = {
                      EDITOR = lib.getExe pkgs.neovim;
                    };
                  })
                  {
                    home = {
                      username = "lukaszczaplinski";
                      homeDirectory = "/home/lukaszczaplinski";
                      stateVersion = "22.11";
                    };
                  }
                ];
                extraSpecialArgs = attrs;
              };
            "lukaszczaplinski@LsGamingDarwin" =
              home-manager.lib.homeManagerConfiguration {
                pkgs = nixpkgs.legacyPackages.x86_64-darwin;
                modules = [
                  ./modules/home.nix
                  ./modules/git.nix
                  (import ./modules/emacs.nix attrs)
                  ./modules/tmux.nix
                  (import ./modules/zsh.nix attrs)
                  ./modules/neovim.nix
                  ({ pkgs, ... }: {
                    programs.emacs.package = pkgs.emacs-macport;
                  })
                  {
                    home = {
                      username = "lukaszczaplinski";
                      homeDirectory = "/Users/lukaszczaplinski";
                      stateVersion = "22.05";
                    };
                  }
                ];
                extraSpecialArgs = attrs;
              };
            "lukaszczaplinski@LsAir" = let system = "aarch64-darwin";
            in home-manager.lib.homeManagerConfiguration {
              pkgs = import nixpkgs {
                inherit system;
                config.allowBroken = true;
              };
              modules = [
                ./modules/cli.nix
                #./modules/ghostty.nix
                #{ config.ghostty.font-size = 16; }
                ./modules/home.nix
                ./modules/llm.nix
                ./modules/wezterm.nix
                (import ./modules/home-manager.nix attrs)
                (import ./modules/emacs.nix attrs)
                ({ pkgs, ... }: {
                  programs.emacs.package = pkgs.emacs-macport.override {
                    withNativeCompilation = false;
                  };
                })
                ./modules/git.nix
                ./modules/tmux.nix
                (import ./modules/zsh.nix attrs)
                ./modules/neovim.nix
                # (import ./modules/\fastanime.nix attrs)
                (import ./modules/nix-search.nix attrs)
                attrs.nix-index-database.homeModules.nix-index
                { programs.nix-index-database.comma.enable = true; }
                {
                  home = {
                    username = "lukaszczaplinski";
                    homeDirectory = "/Users/lukaszczaplinski";
                    stateVersion = "22.05";
                  };
                }
              ];
              extraSpecialArgs = attrs;
            };
            "lukas@LsWootingMBP" = home-manager.lib.homeManagerConfiguration {
              pkgs = import nixpkgs {
                system = "aarch64-darwin";
                config.allowBroken = true;
              };
              modules = [ self.homeManagerModules.wooting ];
              extraSpecialArgs = attrs;
            };
          };
          darwinConfigurations = {
            LsGamingDarwin = darwin.lib.darwinSystem {
              system = "x86_64-darwin";
              modules = [
                ./modules/darwin.nix
                ./modules/darwin/yabai.nix
                ./modules/darwin/sketchybar.nix
                ./modules/darwin/gaming.nix
              ];
            };
            LsWootingMBP = darwin.lib.darwinSystem {
              system = "aarch64-darwin";
              modules = [
                { system.primaryUser = "lukas"; }
                lix
                home-manager.darwinModules.home-manager
                ./modules/darwin.nix
                ./modules/darwin/aerospace.nix
                ./modules/darwin/wooting.nix
                ./modules/darwin/sketchybar.nix
                {
                  users.users.lukas = {
                    name = "lukas";
                    home = "/Users/lukas";
                  };
                }
                {
                  # TODO: these break some thing
                  # read up on them.
                  #home-manager.useGlobalPkgs = true;
                  #home-manager.useUserPackages = true;
                  home-manager.users.lukas = self.homeManagerModules.wooting;
                }
              ];
            };
            LsAir = darwin.lib.darwinSystem {
              system = "aarch64-darwin";
              modules = [
                attrs.lix-module.nixosModules.default
                { system.primaryUser = "lukaszczaplinski"; }
                { ids.gids.nixbld = nixpkgs.lib.mkForce 30000; }
                ./modules/darwin.nix
                ./modules/darwin/aerospace.nix
                ./modules/darwin/sketchybar.nix
                ./modules/darwin/air.nix
                { networking.hostName = "LsAir"; }
              ];
            };
          };

          nixosConfigurations = {
            LsNixOS = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = attrs;
              # Naming is getting fun...
              modules = [ ./modules/framework.nix ./modules/nixos/keyd.nix ];
            };
            LsFramework = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = attrs;
              modules = [
                ./modules/nixos.nix
                ./modules/nixos/smb.nix
                ./modules/nixos/blocky.nix
                ./modules/nixos/jellyfin.nix
                ./modules/nixos/steam.nix
                ./modules/nixos/scrutiny.nix
                ./modules/nixos/yarr.nix
                ./modules/nixos/restic.nix
              ];
            };
            prg-vps-1 = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = attrs;
              modules = [ ./nixos/prg-vps-1/configuration.nix ];
            };
            sd-161581 = nixpkgs.lib.nixosSystem {
              system = "x86_64-linux";
              specialArgs = attrs;
              modules = [ ./nixos/sd-161581/configuration.nix ];
            };
            demo-vm-aarch64-darwin = nixpkgs.lib.nixosSystem {
              system = "aarch64-linux";
              specialArgs = attrs;
              modules = [
                ({ pkgs, ... }: {
                  users.users = {
                    me = {
                      isNormalUser = true;
                      extraGroups = [ "wheel" ];
                    };
                  };

                  virtualisation.vmVariant = {
                    virtualisation = {
                      graphics = false;
                      host.pkgs = nixpkgs.legacyPackages.aarch64-darwin;
                    };
                  };

                  services.openssh = { enable = true; };

                  environment.systemPackages = with pkgs; [ htop ];

                  system.stateVersion = "23.05";
                })
              ];
            };
          };
        };

      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { system, config, pkgs, ... }:
        let
          machine =
            self.nixosConfigurations."demo-vm-${system}".config.system.build.vm;

          program = pkgs.writeShellScript "run-vm.sh" ''
            export NIX_DISK_IMAGE=$(mktemp -u -t nixos.qcow2)

            trap "rm -f $NIX_DISK_IMAGE" EXIT

            ${machine}/bin/run-nixos-vm
          '';
        in {
          apps = {
            default = {
              type = "app";

              program = "${program}";
            };
          };
          devShells.default = pkgs.mkShellNoCC {
            inputsFrom = [ config.just-flake.outputs.devShell ];
            buildInputs = [ pkgs.nixos-rebuild-ng ];
          };
        };
    });
}
