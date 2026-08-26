{
  config,
  lib,
  modulesPath,
  ...
}:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  services.fwupd.enable = true;
  services.fwupd.extraRemotes = [ "lvfs-testing" ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "thunderbolt"
    "uas"
    "usbhid"
    "usb_storage"
    "sd_mod"
    "nvme"
    "cryptd"
  ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [
    "nct6775"
  ];
  boot.extraModulePackages = [ ];
  boot.tmp.useTmpfs = true;
  boot.kernelParams = [ ];

  # https://nixos.wiki/wiki/TPM
  security.tpm2.enable = true;
  security.tpm2.pkcs11.enable = true; # expose /run/current-system/sw/lib/libtpm2_pkcs11.so
  security.tpm2.tctiEnvironment.enable = true; # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables
  users.users.lukaszczaplinski.extraGroups = [ "tss" ]; # tss group has access to TPM devices

  # https://nixos.wiki/wiki/Secure_Boot
  boot.bootspec.enable = true;
  boot = {
    loader.systemd-boot.enable = lib.mkForce false;
    lanzaboote = {
      enable = true;
      pkiBundle = "/etc/secureboot";
    };
  };

  boot.initrd.systemd.services.mount-usb-stick = {
    script = ''
      systemd-mount /dev/disk/by-uuid/b9ee7bed-c893-485c-9dcf-f3e4e3f2ac89 || echo "USB key not found"
    '';
    wantedBy = [ "cryptsetup-pre.target" ];
  };

  boot.initrd.luks.devices = {
    internal = {
      device = "/dev/disk/by-uuid/e9c87df3-a5b1-4765-8da6-d055c26d3323";
      allowDiscards = true;
      keyFile = "/run/media/system/primary/e9c87df3-a5b1-4765-8da6-d055c26d3323.key";
    };
    external = {
      device = "/dev/disk/by-uuid/c5ff78fd-65af-403c-a64b-d80d8192fbf6";
      keyFile = "/run/media/system/primary/c5ff78fd-65af-403c-a64b-d80d8192fbf6.key";
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/6c90233b-786e-4beb-9bee-04040cc8f357";
    fsType = "xfs";
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-uuid/8937c5b3-be18-42fb-b614-fe3f7ae70415";
    fsType = "xfs";
  };

  fileSystems."/var/cache/private/llama-cpp" = {
    device = "/dev/disk/by-uuid/1ff15061-1bbc-49da-9068-01a9db444934";
    fsType = "btrfs";
  };

  fileSystems."/home" = {
    device = "/dev/disk/by-uuid/400c41f6-e1b5-4e1d-822f-79911165008d";
    fsType = "xfs";
  };

  fileSystems."/nix" = {
    device = "/dev/disk/by-uuid/781f7296-2eb9-4cf3-99e7-0e7794fcd1ed";
    fsType = "btrfs";
  };

  fileSystems."/boot/EFI" = {
    device = "/dev/disk/by-uuid/3DF9-C557";
    fsType = "vfat";
  };

  # fileSystems."/srv/nfs" = {
  #   device = "/dev/mapper/SrvVG-nfs";
  #   fsType = "xfs";
  # };

  #swapDevices = [{
  #  device = "/dev/disk/by-partuuid/f50a41ef-4f65-4d47-b7ef-cac669f6bb47";
  #  randomEncryption = true;
  # }];

  # powerManagement.enable = true;
  # powerManagement.cpuFreqGovernor = "ondemand";
  # hardware.cpu.intel.updateMicrocode =
  #   config.hardware.enableRedistributableFirmware;
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  security.pam.services.login.fprintAuth = false;
  security.pam.services.xscreensaver.fprintAuth = false;
  services.fprintd.enable = true;

  services.thermald.enable = true;
  # TODO
  # services.xserver.videoDrivers = lib.mkDefault [ "nvidia" ];
  # hardware.opengl.extraPackages = with pkgs; [ vaapiVdpau ];

  # TODO #2
  #  cryptsetup open /dev/disk/by-uuid/3217d74c-b2bf-4c66-979a-586822df6c51 cryptHdd_mirror1
  #  cryptsetup open /dev/disk/by-uuid/2c48ce2e-2aa6-4557-b71c-14d47805eb87 cryptHdd_mirror3
  #  cryptsetup open /dev/disk/by-uuid/127162ee-cb94-4d6f-a313-15dd7607fdbb cryptHdd_mirror4
  #  mount /dev/mapper/SrvVG-nfs /srv/nfs
}
