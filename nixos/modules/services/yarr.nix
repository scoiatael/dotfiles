{ config, lib, pkgs, ... }:

let port = 7070;
in {
  services.yarr = {
    enable = true;

    inherit port;
    address = "0.0.0.0";
  };
}
