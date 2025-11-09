{ ... }: {
  imports = [
    ./hardware-configuration.nix
    ./networking.nix # generated at runtime by nixos-infect
    ../modules/octocrypt.nix
    ../modules/wh.nix
    ../modules/ip.nix
    ../modules/tailscale.nix
    ../modules/base.nix
    ../modules/security.nix
  ];

  services.nginx = {
    enable = true;

    # enable recommended settings
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    recommendedProxySettings = true;

    proxyCachePath = { "cache" = { enable = true; }; };
  };
  security.acme.defaults.email = "acme@scoiatael.dev";
  security.acme.acceptTerms = true;
  networking.firewall.allowedTCPPorts = [ 80 443 ];

  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = true;
  networking.hostName = "prg-vps-1";
  networking.domain = "scoiatael.omg.lol";
  services.openssh.enable = true;
  users.users.root.openssh.authorizedKeys.keys = [
    "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBHqPfAOEkybKC4aBtpysiR0zqLJGqZfL2JIMhQc5HTR0AOt6MWT3u7RgxqguoZGeLofrA0Egbf55+KSy9+jE/8E= air@scoiatael.dev"
  ];
  system.stateVersion = "23.11";
}
