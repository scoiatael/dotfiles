{
  den.aspects.kde = {
    nixos =
      { pkgs, ... }:
      {
        services.displayManager.sddm = {
          enable = true;
          wayland = {
            enable = true;
            compositor = "weston";
          };
        };
        services.desktopManager.plasma6.enable = true;
        environment.systemPackages = with pkgs; [
          wayland
          libxcb-cursor
          kdePackages.qtsvg
          kdePackages.qtdeclarative
          kdePackages.qt5compat
          kdePackages.qtbase
          kdePackages.qtwayland
          kdePackages.qtmultimedia
        ];
      };
  };
}
