{
  description = "Home-manager configuration";

  inputs.nix-index-database.url = "github:nix-community/nix-index-database";
  inputs.nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
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
  inputs.walker = {
    url = "github:abenz1267/walker";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.doomemacs = {
    url = "github:doomemacs/doomemacs";
    flake = false;
  };
  inputs.lix = {
    url = "https://git.lix.systems/lix-project/lix/archive/main.tar.gz";
    flake = false;
  };
  inputs.lix-module = {
    url = "https://git.lix.systems/lix-project/nixos-module/archive/main.tar.gz";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.lix.follows = "lix";
  };
  inputs.parrhasius = {
    url = "git+https://git.sr.ht/~scoiatael/parrhasius";
    flake = false;
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      darwin,
      ...
    }@attrs:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      homeManagerModules = {
        # Base preset - universal modules for all machines
        base = {
          imports = [
            ./modules/home/default.nix
            ./modules/home/dev/git.nix
            ./modules/home/editors/neovim.nix
          ];
        };

        # CLI tooling preset
        cli-full = {
          imports = [
            ./modules/home/cli.nix
          ];
        };

        # Development stack preset
        dev-full = {
          imports = [
            ./modules/home/multiplexers/tmux.nix
            (import ./modules/home/shells/zsh.nix attrs)
            (import ./modules/home/editors/emacs.nix attrs)
            ./modules/home/terminals/wezterm.nix
            (import ./modules/home/home-manager.nix attrs)
            (import ./modules/home/comma.nix attrs)
          ];
        };

        # Linux-specific preset
        linux-extras = {
          imports = [
            ./modules/platform/linux.nix
            ./modules/platform/electron.nix
          ];
        };

        # Darwin-specific preset
        darwin-extras = {
          imports = [
            ./modules/platform/unfree.nix
            ./modules/platform/secretive.nix
          ];
        };

        wooting = {
          imports = [
            ./modules/platform/unfree.nix
            ./modules/home/default.nix
            ./modules/home/cli.nix
            (import ./modules/home/home-manager.nix attrs)
            ./modules/home/dev/git.nix
            ./modules/home/multiplexers/tmux.nix
            (import ./modules/home/shells/zsh.nix attrs)
            (import ./modules/home/editors/emacs.nix attrs)
            ./modules/home/terminals/wezterm.nix
            ./modules/home/dev/llm.nix
            ./modules/home/editors/neovim.nix
            (import ./modules/home/comma.nix attrs)
            {
              programs.git.settings.user = {
                email = "lukasz@wooting.io";
                name = "Lukas Czaplinski";
                signingkey = "E871295C0EFA7DBFA9E673CC7135745D2C62273D";
              };
            }
            ./modules/git/graphite.nix
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
            self.homeManagerModules.base
            self.homeManagerModules.cli-full
            self.homeManagerModules.dev-full
            self.homeManagerModules.linux-extras
            attrs.walker.homeManagerModules.default
            ./modules/platform/walker.nix
            ./modules/home/terminals/ghostty.nix
            {
              programs.git.settings.user = {
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
        "lukaszczaplinski@LsFramework" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = [
            self.homeManagerModules.base
            self.homeManagerModules.cli-full
            self.homeManagerModules.linux-extras
            ./modules/home/multiplexers/zellij.nix
            ./modules/home/multiplexers/tmux.nix
            (import ./modules/home/shells/zsh.nix attrs)
            (
              { pkgs, lib, ... }:
              {
                programs.zsh.sessionVariables = {
                  EDITOR = lib.getExe pkgs.neovim;
                };
              }
            )
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
        "lukaszczaplinski@LsGamingDarwin" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-darwin;
          modules = [
            self.homeManagerModules.base
            ./modules/home/multiplexers/tmux.nix
            (import ./modules/home/shells/zsh.nix attrs)
            (import ./modules/home/editors/emacs.nix attrs)
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
        "lukaszczaplinski@LsAir" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;
          modules = [
            self.homeManagerModules.base
            self.homeManagerModules.cli-full
            self.homeManagerModules.dev-full
            self.homeManagerModules.darwin-extras
            ./modules/home/dev/llm.nix
            #./modules/home/terminals/ghostty.nix
            #{ config.ghostty.font-size = 16; }
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
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;
          modules = [ self.homeManagerModules.wooting ];
          extraSpecialArgs = attrs;
        };
      };

      darwinConfigurations = {
        LsGamingDarwin = darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          specialArgs = {
            inherit attrs;
            inherit self;
          };
          modules = [ ./darwin/LsGamingDarwin/configuration.nix ];
        };
        LsWootingMBP = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = {
            inherit attrs;
            inherit self;
          };
          modules = [
            home-manager.darwinModules.home-manager
            ./darwin/LsWootingMBP/configuration.nix
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
          specialArgs = attrs;
          modules = [ ./darwin/LsAir/configuration.nix ];
        };
      };

      nixosConfigurations = {
        LsFramework = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = attrs;
          modules = [ ./nixos/framework/configuration.nix ];
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
            (
              { pkgs, ... }:
              {
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

                services.openssh = {
                  enable = true;
                };

                environment.systemPackages = with pkgs; [ htop ];

                system.stateVersion = "23.05";
              }
            )
          ];
        };
      };

      apps = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          machine = self.nixosConfigurations."demo-vm-${system}".config.system.build.vm;
          program = pkgs.writeShellScript "run-vm.sh" ''
            export NIX_DISK_IMAGE=$(mktemp -u -t nixos.qcow2)

            trap "rm -f $NIX_DISK_IMAGE" EXIT

            ${machine}/bin/run-nixos-vm
          '';
        in
        {
          default = {
            type = "app";
            program = "${program}";
          };
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShellNoCC {
            buildInputs = [
              pkgs.nixos-rebuild-ng
              pkgs.just
            ];
          };
        }
      );
    };
}
