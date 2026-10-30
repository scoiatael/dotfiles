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
    "btrfs"
  ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [
    "nct6775"
  ];
  boot.extraModulePackages = [ ];
  boot.tmp.useTmpfs = true;
  boot.kernelParams = [ ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.efiSysMountPoint = "/boot/EFI";

  boot.initrd.luks.devices = {
    internal = {
      device = "/dev/disk/by-uuid/e9c87df3-a5b1-4765-8da6-d055c26d3323";
      allowDiscards = true;
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

  services.thermald.enable = true;
}
