{
  description = "Home-manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    old_nixpkgs.url =
      "github:nixos/nixpkgs/e2dd4e18cc1c7314e24154331bae07df76eb582f";
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
    flake-utils.url = "github:numtide/flake-utils/v1.0.0";
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
      inputs.flake-utils.follows = "flake-utils";
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

  outputs = { self, flake-utils, nixpkgs, home-manager, darwin, emacsMacport
    , ... }@attrs:
    let
      patchedEmacsMacport = { pkgs, ... }: {
        programs.emacs.package =
          # https://bitbucket.org/mituharu/emacs-mac/commits/5f6c306095c825eb01708e336f9d03c15271dfe9
          # see https://github.com/doomemacs/doomemacs/issues/7532
          pkgs.emacs29-macport.overrideAttrs { src = emacsMacport; };
      };
      lix = { pkgs, ... }: { nix.package = pkgs.lix; };
    in {
      homeManagerModules = {
        wooting = let system = "x86_64-darwin";
        in {
          imports = [
            ./modules/home.nix
            ./modules/cli.nix
            (import ./modules/home-manager.nix attrs)
            (import ./modules/git.nix attrs)
            (import ./modules/emacs.nix attrs)
            ./modules/tmux.nix
            ./modules/zsh.nix
            ./modules/neovim.nix
            ./modules/wezterm.nix
            {
              programs.emacs.package =
                attrs.old_nixpkgs.legacyPackages.${system}.emacs29-macport.overrideAttrs {
                  src = emacsMacport;
                };
            }
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
            }
          ];
        };
      };
      homeConfigurations = {
        "L@LsNixOS" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          modules = [
            ./modules/home.nix
            ./modules/electron.nix
            ./modules/linux.nix
            (import ./modules/git.nix attrs)
            ./modules/zsh.nix
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
              ./modules/electron.nix
              ./modules/linux.nix
              (import ./modules/git.nix attrs)
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
              }
            ];
            extraSpecialArgs = attrs;
          };
        "wooting@MacBookPro.bagend" =
          home-manager.lib.homeManagerConfiguration {
            pkgs = import nixpkgs {
              system = "x86_64-darwin";
              config.allowBroken = true;
            };
            modules = [ self.homeManagerModules.wooting ];
            extraSpecialArgs = attrs;
          };
        "lukaszczaplinski@LsGamingDarwin" =
          home-manager.lib.homeManagerConfiguration {
            pkgs = nixpkgs.legacyPackages.x86_64-darwin;
            modules = [
              ./modules/home.nix
              (import ./modules/git.nix attrs)
              (import ./modules/emacs.nix attrs)
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
              }
            ];
            extraSpecialArgs = attrs;
          };
        "lukaszczaplinski@LsAir" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "aarch64-darwin";
            config.allowBroken = true;
          };
          modules = [
            ./modules/cli.nix
            ./modules/home.nix
            ./modules/wezterm.nix
            (import ./modules/home-manager.nix attrs)
            (import ./modules/git.nix attrs)
            (import ./modules/emacs.nix attrs)
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
            }
          ];
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
          system = "x86_64-darwin";
          modules = [
            home-manager.darwinModules.home-manager
            ./modules/darwin.nix
            ./modules/darwin/aerospace.nix
            ./modules/darwin/wooting.nix
            ./modules/darwin/sketchybar.nix
            {
              users.users.wooting = {
                name = "wooting";
                home = "/Users/wooting";
              };
            }
            {
              # TODO: these break some thing
              # read up on them.
              #home-manager.useGlobalPkgs = true;
              #home-manager.useUserPackages = true;
              home-manager.users.wooting = self.homeManagerModules.wooting;
            }
          ];
        };
        LsAir = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            lix
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
          modules = [ ./modules/framework.nix ];
        };
        LsFramework = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = attrs;
          modules = [
            ./modules/nixos.nix
            ./modules/nixos/smb.nix
            ./modules/nixos/jellyfin.nix
            ./modules/nixos/steam.nix
          ];
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
    } // flake-utils.lib.eachDefaultSystem (hostSystem:
      let
        pkgs = nixpkgs.legacyPackages.${hostSystem};
        machine =
          self.nixosConfigurations."demo-vm-${hostSystem}".config.system.build.vm;

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
      });
}
