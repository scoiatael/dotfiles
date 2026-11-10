{
  den.aspects.battle_net = {
    nixos =
      { pkgs, ... }:
      {
        environment.systemPackages = with pkgs; [ faugus-launcher ];
      };
  };
}
