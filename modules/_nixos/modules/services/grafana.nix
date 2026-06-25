{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    {
      systemd.services.grafana.serviceConfig.EnvironmentFile = config.sops.secrets.grafana-env.path;
    }

  ];
  # Default user:password is admin:admin
  # Remember to change it :)
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
      security.secret_key = "$__env{SECRET_KEY}";

      "plugin.marcusolsson-csv-datasource" = {
        allow_local_mode = true;
      };
    };
  };
  systemd = (
    lib.mkIf config.services.scrutiny.enable {
      # Export scrutiny sqlite3 database into JSON format
      services.grafana.serviceConfig.ReadOnlyPaths = [ "/var/lib/export-scrutiny" ];
      services.export-scrutiny =
        let
          runFile = pkgs.writeShellApplication {
            name = "export-scrutiny";
            runtimeInputs = [ pkgs.sqlite ];
            text = ''
              sqlite3 -readonly -header -csv /var/lib/scrutiny/scrutiny.db 'select * from devices;' > devices.csv
            '';
          };
        in
        {
          serviceConfig = {
            ExecStart = lib.getExe runFile;

            Type = "oneshot";
            Restart = "on-failure";
            StateDirectoryMode = "0755";
            StateDirectory = "export-scrutiny";
            WorkingDirectory = "/var/lib/export-scrutiny";
          };
        };
      timers.export-scrutiny = {
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "daily";
        };
      };
    }
  );
}
