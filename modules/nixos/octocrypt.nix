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
    };
  };
}
