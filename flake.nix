{
  description = "Home-manager configuration";

  inputs.nix-index-database.url = "github:nix-community/nix-index-database";
  inputs.nix-index-database.inputs.nixpkgs.follows = "nixpkgs";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
  inputs.home-manager = {
    url = "github:nix-community/home-manager/release-25.11";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.darwin = {
    url = "github:LnL7/nix-darwin/nix-darwin-25.11";
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
  inputs.sops-nix = {
    url = "github:Mic92/sops-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.doomemacs = {
    url = "github:doomemacs/doomemacs";
    flake = false;
  };
  inputs.parrhasius = {
    url = "git+https://git.sr.ht/~scoiatael/parrhasius";
    flake = false;
  };

  outputs =
    {
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
      homeConfigurations = {
        "l@LsNixOS" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = attrs;
          modules = [ ./home/l-LsNixOS/configuration.nix ];
        };
        "lukaszczaplinski@LsFramework" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = attrs;
          modules = [ ./home/lukaszczaplinski-LsFramework/configuration.nix ];
        };
        "lukaszczaplinski@LsGamingDarwin" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-darwin;
          extraSpecialArgs = attrs;
          modules = [ ./home/lukaszczaplinski-LsGamingDarwin/configuration.nix ];
        };
        "lukaszczaplinski@LsAir" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;
          extraSpecialArgs = attrs;
          modules = [ ./home/lukaszczaplinski-LsAir/configuration.nix ];
        };
        "lukas@LsWootingMBP" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;
          extraSpecialArgs = attrs;
          modules = [ ./home/lukas-LsWootingMBP/configuration.nix ];
        };
      };

      darwinConfigurations = {
        LsGamingDarwin = darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          specialArgs = attrs;
          modules = [ ./darwin/LsGamingDarwin/configuration.nix ];
        };
        LsWootingMBP = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = attrs;
          modules = [ ./darwin/LsWootingMBP/configuration.nix ];
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
        # demo-vm-aarch64-darwin = nixpkgs.lib.nixosSystem {
        #   system = "aarch64-linux";
        #   specialArgs = attrs;
        #   modules = [
        #     (
        #       { pkgs, ... }:
        #       {
        #         users.users = {
        #           me = {
        #             isNormalUser = true;
        #             extraGroups = [ "wheel" ];
        #           };
        #         };

        #         virtualisation.vmVariant = {
        #           virtualisation = {
        #             graphics = false;
        #             host.pkgs = nixpkgs.legacyPackages.aarch64-darwin;
        #           };
        #         };

        #         services.openssh = {
        #           enable = true;
        #         };

        #         environment.systemPackages = with pkgs; [ htop ];

        #         system.stateVersion = "23.05";
        #       }
        #     )
        #   ];
        # };
      };

      # apps = forAllSystems (
      #   system:
      #   let
      #     pkgs = nixpkgs.legacyPackages.${system};
      #     machine = self.nixosConfigurations."demo-vm-${system}".config.system.build.vm;
      #     program = pkgs.writeShellScript "run-vm.sh" ''
      #       export NIX_DISK_IMAGE=$(mktemp -u -t nixos.qcow2)

      #       trap "rm -f $NIX_DISK_IMAGE" EXIT

      #       ${machine}/bin/run-nixos-vm
      #     '';
      #   in
      #   {
      #     default = {
      #       type = "app";
      #       program = "${program}";
      #     };
      #   }
      # );

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
