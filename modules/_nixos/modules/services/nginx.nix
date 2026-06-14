{
  config,
  lib,
  ...
}:

{
  # required due to unix socket permissions
  users.users.nginx.extraGroups = lib.mkIf (config.users.groups ? anubis) [
    config.users.groups.anubis.name
  ];

  services.nginx = {
    enable = true;

    # enable recommended settings
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;

    proxyCachePath = {
      "cache" = {
        enable = true;
      };
    };
  };
  security.acme.defaults.email = "acme@scoiatael.dev";
  security.acme.acceptTerms = true;
  networking.firewall.allowedTCPPorts = [
    80
    443
  ];
}
