{ config, lib, pkgs, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "gnome3";
  };
  services.syncthing = {
    enable = true;
    # tray = {
    #     enable = true;
    #     package = pkgs.qsyncthingtray;
    #     command = "qsyncthingtray --wait";
    #   }; doesn't work due to missing 'tray.target'
  };
  services.emacs = { enable = true; };
  home.packages = with pkgs;
    [
      janet # broken on macOS
      procs # broken on macOS
      libnotify # doesn't work on macOS
      lm_sensors # for temp display in i3status-rust?
    ] ++ (with pkgs;
      [
        (nerdfonts.override {
          fonts = [
            "JetBrainsMono" # needed for rofi theme
            "FiraCode"
          ];
        })
      ]);
  fonts.fontconfig.enable = true; # required to autoload fonts from packages

  programs.qutebrowser.enable = true;

  home.file.".pam_environment".text = ''
    GDK_BACKEND=wayland
    QT_WAYLAND_DISABLE_WINDOWDECORATION=1
    _JAVA_AWT_WM_NONREPARENTING=1
    QT_QPA_PLATFORMTHEME="qt5ct"
  '';
}
