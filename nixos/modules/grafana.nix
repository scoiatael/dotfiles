{ config, lib, pkgs, ... }:

{
  services.grafana = {
    enable = true;
    settings = {
      server = {
        # Listening Address
        http_addr = "0.0.0.0";
        # and Port
        http_port = 3000;
        # Grafana needs to know on which domain and URL it's running
        domain = "grafana.heron-pollux.ts.net";
      };
    };
  };
  # Default user:password is admin:admin
  # Remember to change it :)
}
