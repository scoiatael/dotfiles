{
  config,
  lib,
  pkgs,
  ...
}:

{
  services.clamav = {
    updater.enable = true;
    daemon.enable = true;
    scanner = {
      enable = true;
      scanDirectories = [
        "/home"
        "/var/lib"
        "/tmp"
        "/etc"
        "/var/tmp"
        "/srv/nfs/quarantine"
      ];
    };
  };

  # Allow 2 CPUs [[id:c4c4a950-8dbc-4b35-9358-c79c59cb317a][systemd-resource-control]]
  # Otherwise it uses all of them (not very nice!)
  systemd.services.clamav-daemon.serviceConfig = {
    CPUQuota = "200%";
  };
}
