{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.smartmontools ];
  services.scrutiny.enable = true;
  services.scrutiny.collector.enable = true;
  services.scrutiny.openFirewall = true;
  services.scrutiny.influxdb.enable = true;
  services.scrutiny.settings.web.listen.port = 8053;
}
