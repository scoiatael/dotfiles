{ den, ... }: {
  den.aspects.steam = {
    includes = [
      (den.batteries.unfree [
        "steam"
        "steam-original"
        "steam-run"
        "steam-unwrapped"
      ])
    ];

    nixos = {
      programs.steam = {
        enable = true;
      };
    };
  };
}
