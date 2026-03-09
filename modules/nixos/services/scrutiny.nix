{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.smartmontools pkgs.influxdb2 ];

  services.scrutiny.enable = true;
  services.scrutiny.collector.enable = true;
  services.scrutiny.openFirewall = true;
  services.scrutiny.influxdb.enable = true;
  services.scrutiny.settings.web.listen.port = 8053;

  # Tip: remember to change admin password from password12345 :p
  # Tip: create token for telegraf in /root/influx_token
}
