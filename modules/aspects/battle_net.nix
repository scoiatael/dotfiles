{
  den.aspects.battle_net = {
    nixos =
      { pkgs, ... }:
      {
        hardware.graphics.enable32Bit = true;
        # Optional, but recommended for gaming performance:
        programs.gamemode.enable = true;
        environment.systemPackages = with pkgs; [ faugus-launcher ];
      };
  };
}
