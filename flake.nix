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
     {
      homeConfigurations.framework = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
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
        extraSpecialArgs = attrs;
      };
      homeConfigurations.macos = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-darwin;
        modules = [
          ./modules/home.nix
          ./modules/git.nix
          ./modules/emacs.nix
          ./modules/tmux.nix
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
    };
}
