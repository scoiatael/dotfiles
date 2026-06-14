{
  den,
  lib,
  dotfiles,
  ...
}:
{
  den.default.includes = [
    den.batteries.dotfiles
  ];
  _module.args.findDotfile =
    _nixPath: name:
    lib.pipe name [
      (lib.strings.splitString "/")
      (lib.drop 1)
      (
        p:
        if lib.hasAttrByPath p dotfiles then
          lib.attrByPath p null dotfiles
        else
          throw "Couldn't find ${builtins.concatStringsSep "/" p} in dotfiles"
      )
    ];
  _module.args.dotfiles =
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
