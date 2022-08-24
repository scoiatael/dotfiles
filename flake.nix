{
  description = "Home-manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    emacs-overlay = {
      url =
        "github:nix-community/emacs-overlay/e911c43b99c7b9c94ee408c38b0c6e2c6a01132e";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    persway.url =
      "github:scoiatael/persway/f54ef7dbe5f04b43c901f767eb5230db98b9e0ed";
  };

  outputs = { nixpkgs, home-manager, ... }@attrs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in {
      homeConfigurations.framework = home-manager.lib.homeManagerConfiguration {
        inherit pkgs;

        # Specify your home configuration modules here, for example,
        # the path to your home.nix.
        modules = [
          ./modules/home.nix
          ./modules/electron.nix
          ./modules/linux.nix
          ./modules/rclone-gdrive-mount.nix
          ./modules/sway.nix
          ./modules/git.nix
          ./modules/emacs.nix
          ./modules/tmux.nix
          {
            home = {
              username = "lczaplinski";
              homeDirectory = "/home/lczaplinski";
              stateVersion = "21.11";
            };
            programs.home-manager.enable = true;
          }
        ];

        # Optionally use extraSpecialArgs
        # to pass through arguments to home.nix
        extraSpecialArgs = attrs;
      };
    };
}
