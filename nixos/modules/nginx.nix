{ config, lib, pkgs, ... }:

{

  services.nginx = {
    enable = true;

    # enable recommended settings
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;

    proxyCachePath = { "cache" = { enable = true; }; };
  };
  security.acme.defaults.email = "acme@scoiatael.dev";
  security.acme.acceptTerms = true;
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
