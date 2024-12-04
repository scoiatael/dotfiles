{ config, lib, pkgs, modulesPath, nixos-hardware, lanzaboote, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
    nixos-hardware.nixosModules.framework-11th-gen-intel
  ];

  boot.initrd.availableKernelModules = [
    "xhci_pci"
    "thunderbolt"
    "uas"
    "usbhid"
    "usb_storage"
    "sd_mod"
    "nvme"
    "aesni_intel"
    "cryptd"
  ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.tmp.useTmpfs = true;

  # https://nixos.wiki/wiki/TPM
  security.tpm2.enable = true;
  security.tpm2.pkcs11.enable =
    true; # expose /run/current-system/sw/lib/libtpm2_pkcs11.so
  security.tpm2.tctiEnvironment.enable =
    true; # TPM2TOOLS_TCTI and TPM2_PKCS11_TCTI env variables
  users.users.l.extraGroups = [ "tss" ]; # tss group has access to TPM devices

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/b4c6b5e9-4279-44aa-ae78-f36c38fcc6d4";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."luks-716c5155-5713-42ab-8289-65be33f99623".device =
    "/dev/disk/by-uuid/716c5155-5713-42ab-8289-65be33f99623";
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/6B41-D922";
    fsType = "vfat";
    options = [ "fmask=0077" "dmask=0077" ];
  };

  powerManagement.enable = true;
  powerManagement.cpuFreqGovernor = "ondemand";

  security.pam.services.login.fprintAuth = false;
  security.pam.services.xscreensaver.fprintAuth = false;
  services.fprintd.enable = true;

  services.thermald.enable = true;

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp170s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
