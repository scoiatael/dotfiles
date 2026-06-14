{ inputs, ... }:
{
  den.hosts.x86_64-linux.sd-161581 = { };
  den.aspects.sd-161581 = {
    nixos = {
      imports = [
        inputs.sops-nix.nixosModules.sops
        ../nixos/sd-161581/hardware-configuration.nix
        ../nixos/sd-161581/networking.nix # generated at runtime by nixos-infect
        ../nixos/modules/cli.nix
        ../nixos/modules/tailscale.nix
        ../nixos/modules/base.nix
        ../nixos/modules/security.nix
        ../nixos/modules/services/wh.nix
        ../nixos/modules/services/nginx.nix
        ../nixos/modules/services/octocrypt.nix
        ../nixos/modules/ip.nix
        ../nixos/modules/services/telegraf.nix
        ../nixos/modules/services/magic-wormhole.nix
        ../nixos/modules/services/prism-tools.nix
      ];

      sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      sops.secrets.influx-token = {
        sopsFile = ../nixos/sd-161581/secrets/influx-token;
        format = "binary";
      };

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
    };
  };
}
