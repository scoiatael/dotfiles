{ self', pkgs, ... }:

{
  home.packages = with pkgs; [
    wofi
    self'.packages.gtklock
  ];

  xdg.configFile."newm/config.py".source = ../config/newm/config.py;
}
