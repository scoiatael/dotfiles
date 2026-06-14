{ config, ... }:

{
  homebrew = {
    brews = [ "acsandmann/tap/rift" ];
  };
  launchd.user.agents."git.acsandmann.rift" = {
    serviceConfig = {
      Label = "git.acsandmann.rift";
      ProgramArguments = [ "/opt/homebrew/bin/rift" ];
      EnvironmentVariables = {
        PATH = builtins.concatStringsSep ":" [
          "/Users/${config.system.primaryUser}/.nix-profile/bin"
          "/run/current-system/sw/bin"
          "/nix/var/nix/profiles/default/bin"
          "/usr/local/bin"
          "/usr/bin"
          "/bin"
          "/usr/sbin"
          "/sbin"
          "/opt/homebrew/bin/"
        ];
        RUST_LOG = "error,warn,info";
      };
      RunAtLoad = true;
      KeepAlive = {
        SuccessfulExit = false;
        Crashed = true;
      };
      StandardOutPath = "/tmp/rift_${config.system.primaryUser}.out.log";
      StandardErrorPath = "/tmp/rift_${config.system.primaryUser}.err.log";
      ProcessType = "Interactive";
      LimitLoadToSessionType = "Aqua";
      Nice = -20;
    };
  };

  system.defaults.spaces.spans-displays = false;

  home-manager.users.${config.system.primaryUser}.imports = [
    {
      home.file.".config/rift/config.toml" = {
        source = ./config.toml;
      };
    }
  ];
}
