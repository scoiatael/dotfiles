{
  dotfiles,
  findDotfile,
  lib,
  ...
}:
let
  mkAspect = class: {
    ${class}._module.args = {
      inherit dotfiles findDotfile;
      __findFile = findDotfile;
    };
  };

  osAspect =
    { host }:
    {
      name = "dotfiles/os";
    }
    # Guard a synthetic host identity (classless `user@host` home) the same way
    # hmAspect already guards `home ? class`.
    // lib.optionalAttrs (host ? class) (mkAspect host.class);

  userAspect =
    {
      user,
    }:
    {
      name = "dotfiles/user";
      includes = map (c: mkAspect c) user.classes;
    };

  hmAspect =
    { home }:
    {
      name = "dotfiles/home";
    }
    // lib.optionalAttrs (home ? class) (mkAspect home.class);

in
{
  den.batteries.dotfiles = {
    name = "dotfiles'";
    includes = [
      osAspect
      userAspect
      hmAspect
    ];
  };
}
