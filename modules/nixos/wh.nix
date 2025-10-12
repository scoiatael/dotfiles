{ config, lib, pkgs, ... }:

let
  drv = pkgs.callPackage ../../packages/wh { };
  build = pkgs.writeShellScript "build-wh" ''
    set -exuo pipefail
    cd /run/wh-build
    cp -r ${drv}/deps.edn ./
    cp -r ${drv}/src ./
    cp -r ${drv}/build* ./
    ${pkgs.clojure}/bin/clj -T:build uber
    ${pkgs.graalvmPackages.graalvm-ce}/bin/native-image \
         --report-unsupported-elements-at-runtime \
         -H:-CheckToolchain \
         --initialize-at-build-time \
         --no-server \
         -jar ./target/lib1-0.0.0-standalone.jar \
         -H:Name=./target/hello-world
  '';
in {
  systemd.services.wh = {
    enable = true;
    description = "Webhook2RSS Web service";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      ExecStart = "/var/cache/target/hello-world";
      ExecStartPre = build;
      ExecPaths = "/run/wh-build";
      DynamicUser = true;
      Restart = "on-failure";
      CacheDirectory = "wh";
      StateDirectory = "wh";
      RuntimeDirectory = "wh-build";
      Environment = [
        "HOME=/var/cache/wh"
        "DB=/var/lib/wh/db.sqlite3"
        "PORT=3001"
        "PROXY=true"
      ];
      TimeoutStartSec = "infinity";
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
