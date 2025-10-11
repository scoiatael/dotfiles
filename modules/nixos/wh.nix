{ config, lib, pkgs, ... }:

let drv = pkgs.callPackage ../../packages/wh { };
in {
  systemd.services.wh = {
    enable = true;
    description = "Webhook2RSS Web service";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      ExecStart = "${drv}/bin/wh";
      DynamicUser = true;
      Restart = "on-failure";
      CacheDirectory = "wh";
      StateDirectory = "wh";
      Environment = [
        "HOME=/var/cache/wh"
        "DB=/var/lib/wh/db.sqlite3"
        "PORT=3001"
        "BABASHKA_PODS_DIR=/var/cache/wh/pods"
        "PROXY=true"
      ];
      # Required for babashka pods to work
      ExecPaths = "/var/cache/wh/pods";
      MemoryMax = "300M";
    };
  };
  services.nginx.virtualHosts."wh.scoiatael.dev" = {
    forceSSL = true;
    enableACME = true;
    extraConfig = ''
      error_page 500 502 503 504 =200 ${pkgs.nginx}/html/50x.html;
      proxy_cache cache;
      add_header X-Cache $upstream_cache_status;
    '';
    locations = {
      "/".proxyPass =
        "http://unix:${config.services.anubis.instances.wh.settings.BIND}";
      "/webhook".proxyPass = "http://localhost:3001";
      "~ .css".root = "${drv}/public";
    };
  };
  services.anubis.instances.wh.settings.TARGET = "http://localhost:3001";
}
