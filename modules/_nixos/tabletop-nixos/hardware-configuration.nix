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

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "ahci"
    "nvme"
    "usb_storage"
    "usbhid"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/mapper/luks-9bf9c3a9-fb28-4c71-bdd1-de8204f7add7";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."luks-9bf9c3a9-fb28-4c71-bdd1-de8204f7add7".device =
    "/dev/disk/by-uuid/9bf9c3a9-fb28-4c71-bdd1-de8204f7add7";

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/82D9-66E7";
    fsType = "vfat";
    options = [
      "fmask=0077"
      "dmask=0077"
    ];
  };

  swapDevices = [
    { device = "/dev/mapper/luks-635483c0-a7da-41ea-815c-67af200fc196"; }
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
