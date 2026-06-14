{
  config,
  lib,
  pkgs,
  ...
}:

{
  environment.systemPackages = [ pkgs.rocmPackages.rocm-smi ];
  services.telegraf.extraConfig.inputs.amd_rocm_smi = { };
  systemd.services.telegraf.path = [ pkgs.rocmPackages.rocm-smi ];
}
