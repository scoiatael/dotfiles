{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    defaultGateway = "86.54.82.1";
    defaultGateway6 = {
      address = "";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [{
          address = "86.54.82.55";
          prefixLength = 24;
        }];
        ipv6.addresses = [{
          address = "fe80::6b1f:cb77:bde9:4bac";
          prefixLength = 64;
        }];
        ipv4.routes = [{
          address = "86.54.82.1";
          prefixLength = 32;
        }];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="00:16:3e:3f:ca:3d", NAME="eth0"
  '';
}
