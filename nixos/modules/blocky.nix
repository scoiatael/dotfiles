{ ... }: {
  services.blocky = {
    enable = true;
    settings = {
      ports.dns = 53; # Port for incoming DNS Queries.
      upstreams.groups.default = [ "https://dns.quad9.net/dns-query" ];
      # For initially solving DoH/DoT Requests when no system Resolver is available.
      bootstrapDns = {
        upstream = "https://dns.quad9.net/dns-query";
        ips = [ "9.9.9.9" "149.112.112.112" ];
      };
      #Enable Blocking of certian domains.
      blocking = {
        blackLists = {
          #Adblocking
          ads = [
            "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"
          ];
        };
        #Configure what block categories are used
        clientGroupsBlock = { default = [ "ads" ]; };
      };
      # This would work if LAN included tailscale routing
      # conditional = {
      #   fallbackUpstream = false;
      #   # Resolve tailscale addresses
      #   mapping."ts.net" = "100.100.100.100";
      # };
      customDNS.mapping = {
        "parrhasius.heron-pollux.ts.net" = "192.168.1.153";
        "yarr.heron-pollux.ts.net" = "192.168.1.153";
        "scrutiny.heron-pollux.ts.net" = "192.168.1.153";
        "jellyfin.heron-pollux.ts.net" = "192.168.1.153";
        "influxdb.heron-pollux.ts.net" = "192.168.1.153";
      };
    };
  };
  networking.firewall.allowedUDPPorts = [ 53 ];
  networking.firewall.allowedTCPPorts = [ 53 ];
}
