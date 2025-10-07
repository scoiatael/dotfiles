{ config, lib, pkgs, ... }:

let port = 7070;
in {
  services.yarr = {
    enable = true;

    inherit port;
    address = "192.168.1.153";
  };

  networking.firewall.allowedTCPPorts = [ port ];
}
