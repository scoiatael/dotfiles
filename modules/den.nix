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
  den.default.includes = [ den.batteries.self' ];
  den.schema.user.classes = lib.mkDefault [ "homeManager" ];
}
