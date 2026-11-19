{
  den.aspects.incus = {
    nixos =
      { self', ... }:
      {
        # incus-admin is root-equivalent on the host; the including host sets who may SSH in.
        users.users.incant = {
          isNormalUser = true;
          description = "Shared user for incant VMs";
          extraGroups = [ "incus-admin" ];
          packages = [ self'.packages.incant ];
        };
        virtualisation.incus = {
          enable = true;
          preseed = {
            networks = [
              {
                name = "incusbr0";
                type = "bridge";
                config = {
                  "ipv4.address" = "10.0.100.1/24";
                  "ipv4.nat" = "true";
                  "ipv6.address" = "none";
                };
              }
            ];
            storage_pools = [
              {
                name = "default";
                driver = "dir";
              }
            ];
            profiles = [
              {
                name = "default";
                devices = {
                  eth0 = {
                    name = "eth0";
                    network = "incusbr0";
                    type = "nic";
                  };
                  root = {
                    path = "/";
                    pool = "default";
                    type = "disk";
                  };
                };
              }
            ];
          };
        };
        # Incus on NixOS requires the nftables firewall backend.
        networking.nftables.enable = true;
        networking.firewall.trustedInterfaces = [ "incusbr0" ];
      };
  };
}
