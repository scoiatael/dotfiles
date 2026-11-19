{
  den.aspects.keyboard-pl = {
    nixos = {
      console.keyMap = "pl2";
      # SDDM's Wayland greeter takes its keymap from here despite the xserver prefix.
      services.xserver.xkb.layout = "pl";
    };
    homeManager = {
      xdg.configFile."kxkbrc".text = ''
        [Layout]
        LayoutList=pl
        Use=true
      '';
    };
  };
}
