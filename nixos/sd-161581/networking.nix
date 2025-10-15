{ lib, ... }: {
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "51.159.47.26"
      "51.159.47.28"

    ];
    defaultGateway = "51.159.59.1";
    defaultGateway6 = {
      address = "fe80::a293:51ff:fea2:ded5";
      interface = "enp1s0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce true;
    interfaces = {
      enp1s0 = {
        ipv4.addresses = [{
          address = "51.159.59.215";
          prefixLength = 24;
        }];
        ipv6.addresses = [
          {
            address = "2001:bc8:1200:8:d2da:5a78:9adf:6195";
            prefixLength = 64;
          }
          {
            address = "fe80::debc:f780:5e9d:ff9e";
            prefixLength = 64;
          }
        ];
        ipv4.routes = [{
          address = "51.159.59.1";
          prefixLength = 32;
        }];
        ipv6.routes = [{
          address = "fe80::a293:51ff:fea2:ded5";
          prefixLength = 128;
        }];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="00:08:a2:0c:77:ae", NAME="enp1s0"
  '';
}
