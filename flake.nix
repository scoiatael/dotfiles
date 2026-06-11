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
  inputs.import-tree.url = "github:denful/import-tree";
  inputs.den.url = "github:denful/den";

  outputs =
    {
      nixpkgs,
      home-manager,
      darwin,
      import-tree,
      ...
    }@attrs:
    let
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      den =
        (nixpkgs.lib.evalModules {
          modules = [ (import-tree ./modules) ];
          specialArgs.inputs = attrs;
        }).config;

      inherit (den.den.hosts.aarch64-darwin) lsair;
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
        "lukas@LsWootingMBP" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.aarch64-darwin;
          extraSpecialArgs = attrs;
          modules = [ ./home/lukas-LsWootingMBP/configuration.nix ];
        };
      };

      darwinConfigurations = {
        LsWootingMBP = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = attrs;
          modules = [ ./darwin/LsWootingMBP/configuration.nix ];
        };
        LsAir = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = attrs;
          modules = [ lsair.mainModule ];
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
        tabletop-nixos = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = attrs;
          modules = [ ./nixos/tabletop-nixos/configuration.nix ];
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
