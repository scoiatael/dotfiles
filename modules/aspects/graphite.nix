{
  den,
  ...
}:
{
  den.aspects.graphite = {
    includes = [
      (den.batteries.unfree [
        "graphite-cli"
      ])
    ];
    homeManager = { pkgs, ... }: {
      home.packages = [ pkgs.graphite-cli ];
    };
  };
}
