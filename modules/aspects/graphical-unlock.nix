{
  # Plymouth LUKS prompt the way Fedora/Bazzite get it: systemd-cryptsetup asks, Plymouth's agent draws the box.
  # Hosts must load their GPU driver in the initrd, or Plymouth has no framebuffer yet and falls back to text.
  den.aspects.graphical-unlock = {
    nixos = {
      boot.initrd.systemd.enable = true;
      boot.plymouth = {
        enable = true;
        # Bazzite's value; slow GPUs otherwise miss the deadline and get the text prompt.
        extraConfig = "DeviceTimeout=15";
      };
      boot.kernelParams = [ "quiet" ];
      boot.consoleLogLevel = 0;
      boot.initrd.verbose = false;
    };
  };
}
