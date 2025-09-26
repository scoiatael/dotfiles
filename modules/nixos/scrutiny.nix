{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.smartmontools pkgs.influxdb2 ];

  services.scrutiny.enable = true;
  services.scrutiny.collector.enable = true;
  services.scrutiny.openFirewall = true;
  services.scrutiny.influxdb.enable = true;
  services.scrutiny.settings.web.listen.port = 8053;

  # Open influxdb port; Scrutiny will setup user / password
  # Tip: remember to change admin password from password12345 :p
  # Tip: create token for telegraf in /root/influx_token
  networking.firewall.allowedTCPPorts = [ 8086 ];

  systemd.services.telegraf.serviceConfig.EnvironmentFile =
    "/root/influx_token";
  services.telegraf.enable = true;
  services.telegraf.extraConfig = {
    outputs.influxdb_v2 = {
      urls = [ "http://localhost:8086" ];
      token = "\${INFLUX_TOKEN}";
      bucket = "telegraf";
      organization = "scrutiny";
    };
    inputs = {
      diskio = { };
      kernel_vmstat = { };
      system = { };
      mem = { };
      # TODO: monitor listed systemd units (add options)
      # systemd_units = { };
      swap = { };
      cpu = { };
      disk = { };

      exec = let
        nixosSystems = pkgs.writeShellScript "current-system" ''
          printf "nixos_systems,current_system=%s,booted_system=%s,current_kernel=%s,booted_kernel=%s present=0\n" \
            "$(readlink /run/current-system)" "$(readlink /run/booted-system)" \
            "$(basename $(echo /run/current-system/kernel-modules/lib/modules/*))" \
            "$(basename $(echo /run/booted-system/kernel-modules/lib/modules/*))"
        '';
      in [{
        # Expose the path to current-system as metric. We use
        # this to check if the machine is up-to-date.
        commands = [ nixosSystems ];
        data_format = "influx";
      }];
    };
  };
}
