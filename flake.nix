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
      inherit (den.den.hosts.aarch64-darwin) lswootingmbp;
      inherit (den.den.hosts.x86_64-linux) lsframework;
      inherit (den.den.hosts.x86_64-linux) sd-161581;
      inherit (den.den.hosts.x86_64-linux) tabletop-nixos;
    in
    {
      darwinConfigurations = {
        LsWootingMBP = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = attrs;
          modules = [ lswootingmbp.mainModule ];
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
          modules = [ lsframework.mainModule ];
        };
        sd-161581 = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = attrs;
          modules = [ sd-161581.mainModule ];
        };
        tabletop-nixos = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          specialArgs = attrs;
          modules = [ tabletop-nixos.mainModule ];
        };
      };

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
