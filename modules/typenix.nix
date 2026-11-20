{
  lib,
  inputs,
  ...
}:
{
  flake-file.inputs.typenix.url = "github:ryanrasti/typenix";

  # Type-checks packages/ against packages/pkgs.d.ts; see packages/tsconfig.json.
  flake.checks = lib.genAttrs (builtins.attrNames inputs.typenix.packages) (
    system:
    let
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      typenix = inputs.typenix.packages.${system}.default;
    in
    {
      typenix = pkgs.runCommand "typenix-packages" { } ''
        cd ${../packages}
        ${lib.getExe' typenix "typenix"} --noEmit --pretty false
        touch $out
      '';
    }
  );
}
