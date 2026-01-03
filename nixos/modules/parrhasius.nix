{ config, lib, pkgs, parrhasius, ... }:

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
      pkgs.yarn
      pkgs.nodejs
      pkgs.sqlite
      pkgs.gnumake
      pkgs.gcc
      pkgs.gnutar
      pkgs.gzip
      pkgs.pkg-config
    ];

    preStart = ''
      cp -r --no-preserve=mode -t ./ ${parrhasius}/*
      chmod u+w -R ./
      bundle install
      bundle exec rake build
    '';

    script = ''
      bundle exec puma --bind=tcp://0.0.0.0:4567 -C config/puma.rb
    '';

    environment = {
      SERVE = "/srv/nfs/downloads/parrhasius.db/";
      RAILS_ENV = "production";
      RACK_ENV = "production";
      C_INCLUDE_PATH = "${pkgs.libyaml.dev}/include";
      LIBRARY_PATH = "${pkgs.libyaml}/lib";
      SOLID_QUEUE_IN_PUMA = 1;
      WEB_CONCURRENCY = 4;
    };

    serviceConfig = {
      Type = "simple";
      WorkingDirectory = config.users.users.parrhasius.home;
      Restart = "on-failure";
      User = config.users.users.parrhasius.name;

      EnvironmentFile = [ "/etc/nixos/secrets/parrhasius.env" ];
    };
  };
}
