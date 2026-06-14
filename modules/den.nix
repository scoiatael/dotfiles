{
  den,
  inputs,
  lib,
  ...
}:
{
  flake-file.inputs = {
    den.url = "github:denful/den";
    import-tree.url = "github:denful/import-tree";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  imports = [ inputs.den.flakeModule ];
  den.default.includes = [
    den.batteries.self'
    den.batteries.dotfiles
  ];
  den.schema.user.classes = lib.mkDefault [ "homeManager" ];

  den.aspects.dotfiles =
    let
      exposeDir =
        { dir, ... }@attrs:
        let
          files = lib.filesystem.listFilesRecursive dir;
        in
        lib.genAttrs' files (p: {
          name = lib.pipe (toString p) [
            (lib.strings.removePrefix (toString dir + "/"))
            (lib.strings.removeSuffix (attrs.removeSuffix or ""))
          ];
          value = p;
        });
    in
    {
      config = exposeDir { dir = ../config; };
      homeModules = exposeDir {
        dir = ./_home/modules;
        removeSuffix = ".nix";
      };
      darwinModules = exposeDir {
        dir = ./_darwin/modules;
        removeSuffix = ".nix";
      };
      nixosModules = exposeDir {
        dir = ./_nixos/modules;
        removeSuffix = ".nix";
      };
    };
}
