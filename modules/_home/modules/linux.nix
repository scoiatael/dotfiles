{ pkgs, ... }:

{
  # services.gpg-agent = {
  #   enable = true;
  #   enableSshSupport = true;
  #   pinentryPackage = pkgs.pinentry-qt;
  # };

  home.packages = with pkgs; [
    janet # broken on macOS
    procs # broken on macOS
    libnotify # doesn't work on macOS
    lm_sensors # for temp display in i3status-rust?
    librewolf
  ];
  services.unclutter.enable = true;
  xresources.properties = {
    "Xft.antialias" = true;
    "Xft.autohint" = false;
    "Xft.rgba" = "rgb";
    "Xft.hinting" = true;
    "Xft.hintstyle" = "hintfull";
    "Xft.lcdfilter" = "lcddefault";
  };
  programs.wezterm.enable = true;
}
