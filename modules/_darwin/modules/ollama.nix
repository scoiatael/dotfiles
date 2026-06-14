{ config, lib, pkgs, ... }:

{
  environment.systemPackages = [ pkgs.ollama ];
  launchd.user.agents.ollama = {
    path = [ config.environment.systemPath ];
    serviceConfig.ProgramArguments = [ "${pkgs.ollama}/bin/ollama" "serve" ];
    serviceConfig.KeepAlive = true;
    serviceConfig.RunAtLoad = true;
  };
}
