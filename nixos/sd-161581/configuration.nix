{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix # generated at runtime by nixos-infect
    ../../modules/nixos/cli.nix
    ../../modules/nixos/tailscale.nix
    ../../modules/nixos/base.nix
    ../../modules/nixos/security/security.nix
    ../../modules/nixos/services/wh.nix
    ../../modules/nixos/services/nginx.nix
    ../../modules/nixos/services/octocrypt.nix
    ../../modules/nixos/ip.nix
    ../../modules/nixos/services/telegraf.nix
    ../../modules/nixos/services/magic-wormhole.nix
  ];

  services.kubo.enable = true;

  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = false;
  networking.hostName = "sd-161581";
  networking.domain = "scoiatael.omg.lol";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHqPfAOEkybKC4aBtpysiR0zqLJGqZfL2JIMhQc5HTR0AOt6MWT3u7RgxqguoZGeLofrA0Egbf55+KSy9+jE/8E= air@scoiatael.dev"
  ];
  system.stateVersion = "23.11";
}
