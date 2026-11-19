{
  den.aspects.caps2esc = {
    nixos =
      { pkgs, ... }:
      {
        # https://github.com/NixOS/nixpkgs/issues/126681
        services.interception-tools = {
          enable = true;
          plugins = [ pkgs.interception-tools-plugins.caps2esc ];
          udevmonConfig = ''
            - JOB: "${pkgs.interception-tools}/bin/intercept -g $DEVNODE | ${pkgs.interception-tools-plugins.caps2esc}/bin/caps2esc -m 1 | ${pkgs.interception-tools}/bin/uinput -d $DEVNODE"
              DEVICE:
                EVENTS:
                  EV_KEY: [KEY_CAPSLOCK, KEY_ESC]
          '';
        };
      };
  };
}
