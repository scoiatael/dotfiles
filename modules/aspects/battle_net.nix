{
  den.aspects.battle_net = {
    nixos =
      { pkgs, ... }:
      {
        programs.nix-ld.enable = true;
        hardware.graphics.enable32Bit = true;
        # Optional, but recommended for gaming performance:
        programs.gamemode.enable = true;
        environment.systemPackages = with pkgs; [
          lutris
          wineWow64Packages.staging
          winetricks
        ];
      };
  };
}
