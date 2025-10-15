{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix # generated at runtime by nixos-infect
    ../../modules/nixos/cli.nix
  ];

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
