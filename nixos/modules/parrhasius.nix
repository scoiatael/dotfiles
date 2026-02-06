{
  config,
  lib,
  pkgs,
  parrhasius,
  ...
}:

let
  environmentFile = pkgs.writeTextFile {
    name = "parrhasius.env";
    text = ''
      SERVE="/srv/nfs/downloads/parrhasius.db/"
      RAILS_ENV="production"
      RACK_ENV="production"
      SOLID_QUEUE_IN_PUMA="1"
      C_INCLUDE_PATH="${pkgs.libyaml.dev}/include"
      LIBRARY_PATH="${pkgs.libyaml}/lib"
      WEB_CONCURRENCY="4"
      BUNDLE_HOME="/var/lib/parrhasius-gems"
      DATABASE_URL="sqlite3:///var/lib/parrhasius-db/production.sqlite3"
    '';
  };
in
{
  users.users.parrhasius = {
    isSystemUser = true;
    home = "/var/lib/parrhasius";
    createHome = true;
    group = "parrhasius";
    shell = "/run/current-system/sw/bin/nologin";
  };
  users.groups.parrhasius = { };

  systemd.services.parrhasius = {
    description = "Parrhasius";
    wantedBy = [ "default.target" ];
    path = [
      pkgs.ruby_3_3.devEnv
      pkgs.go
      pkgs.bun
      pkgs.sqlite
      pkgs.gnumake
      pkgs.gcc
      pkgs.gnutar
      pkgs.gzip
      pkgs.pkg-config
      pkgs.imagemagick
    ];

    preStart = ''
      cp -r --no-preserve=mode -t ./ ${parrhasius}/*
      chmod u+w -R ./
      bundle install
      bundle exec rake build
      bundle exec rake db:migrate
    '';

    script = ''
      bundle exec puma --bind=tcp://0.0.0.0:4567 -C config/puma.rb
    '';

    environment = {
    };

    serviceConfig = {
      Type = "simple";
      WorkingDirectory = config.users.users.parrhasius.home;
      Restart = "on-failure";
      User = config.users.users.parrhasius.name;
      StateDirectory = "parrhasius-db parrhasius-gems parrhasius-bun";
      EnvironmentFile = [
        "/etc/nixos/secrets/parrhasius.env"
        environmentFile
      ];
    };
  };
}
