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
    "nvme"
    "xhci_pci"
    "ahci"
    "usb_storage"
    "usbhid"
    "sd_mod"
  ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  fileSystems."/" = {
    device = "/dev/mapper/luks-56ec6f15-8523-43f4-821a-27e0e5b5853e";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."luks-56ec6f15-8523-43f4-821a-27e0e5b5853e".device =
    "/dev/disk/by-uuid/56ec6f15-8523-43f4-821a-27e0e5b5853e";

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C0BE-B503";
    fsType = "vfat";
    options = [
      "fmask=0077"
      "dmask=0077"
    ];
  };

  swapDevices = [
    { device = "/dev/mapper/luks-b9dd291f-5012-47fe-85fd-fdd178db0717"; }
  ];

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
