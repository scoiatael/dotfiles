{ config, lib, pkgs, ... }:

{
  # required due to unix socket permissions
  users.users.nginx.extraGroups = [ config.users.groups.anubis.name ];
  services.nginx.virtualHosts."octocrypt.scoiatael.dev" = {
    forceSSL = true;
    enableACME = true;
    locations = {
      "/".proxyPass =
        "http://unix:${config.services.anubis.instances.default.settings.BIND}";
    };
  };
}
