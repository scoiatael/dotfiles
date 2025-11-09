{ config, lib, pkgs, ... }:

let drv = pkgs.callPackage ../../packages/ip { };
in {
  systemd.services.ip = {
    enable = true;
    description = "MyIP Web service";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      ExecStart = "${drv}/bin/ip";
      DynamicUser = true;
      Restart = "on-failure";
      Environment = [ "PORT=3002" "PROXY=true" ];
      MemoryMax = "100M";
    };
  };
  services.nginx.virtualHosts."ip.scoiatael.dev" = {
    forceSSL = true;
    enableACME = true;
    extraConfig = ''
      error_page 500 502 503 504 =200 ${pkgs.nginx}/html/50x.html;
      proxy_cache cache;
      add_header X-Cache $upstream_cache_status;
    '';
    locations = {
      "/".proxyPass =
        "http://unix:${config.services.anubis.instances.ip.settings.BIND}";
    };
  };
  services.anubis.instances.ip.settings.TARGET = "http://localhost:3002";
}
