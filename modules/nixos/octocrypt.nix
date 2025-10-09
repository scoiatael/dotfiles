{ config, lib, pkgs, ... }:

let
  drv = pkgs.callPackage ../../packages/octocrypt { };
  logConfFile = pkgs.writeText "octocrypt-log4j2.properties" ''
    rootLogger.level = info
    rootLogger.appenderRef.stdout.ref = STDOUT

    appender.console.type = Console
    appender.console.name = STDOUT
    appender.console.filter.threshold.type = ThresholdFilter
    appender.console.filter.threshold.level = info
    appender.console.layout.type = PatternLayout
    appender.console.layout.pattern = [%c] %x %X %highlight{%m}%n
  '';
in {
  systemd.services.octocrypt = {
    enable = true;
    description = "Octocrypt Web service";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    serviceConfig = {
      ExecStart = "${drv}/bin/octocrypt";
      DynamicUser = true;
      Restart = "on-failure";
      CacheDirectory = "octocrypt";
      Environment = [
        "HOME=/var/cache/octocrypt"
        "LOG4J_CONFIGURATION_FILE=${logConfFile}"
      ];
      MemoryMax = "300M";
    };
  };

  # required due to unix socket permissions
  users.users.nginx.extraGroups = [ config.users.groups.anubis.name ];
  services.nginx.virtualHosts."octocrypt.scoiatael.dev" = {
    forceSSL = true;
    enableACME = true;
    extraConfig = ''
      error_page 500 502 503 504 =200 ${pkgs.nginx}/html/50x.html;
      proxy_cache cache;
      add_header X-Cache $upstream_cache_status;
    '';
    locations = {
      "/".proxyPass =
        "http://unix:${config.services.anubis.instances.default.settings.BIND}";
    };
  };

  services.anubis.instances.default.settings.TARGET = "http://localhost:3000";
}
