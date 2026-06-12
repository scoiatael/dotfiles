{
  lib,
  inputs,
  ...
}:
{

  flake.devShells = lib.genAttrs lib.systems.flakeExposed (
    system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      auto-follow = inputs.nix-auto-follow.packages.${system}.default;
    in
    {
      default = pkgs.mkShellNoCC {
        buildInputs = [
          pkgs.nixos-rebuild-ng
          pkgs.just
          auto-follow
        ];
      };
    }
  );
}
