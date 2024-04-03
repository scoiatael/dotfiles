{ config, lib, pkgs, ... }:

{
  home.file.".hammerspoon/Spoons" = {
    source = ../config/Spoons;
    recursive = true;
  };
  # TODO: Port more stuff from https://github.com/rtauziac/Hammerspoon-Yabai/blob/master/.hammerspoon/init.lua
  # TODO: Port more stuff from https://github.com/treffynnon/nix-setup/blob/master/home-configs/hammerspoon/init.lua
  home.file.".hammerspoon/init.lua".text = ''
    hs.loadSpoon("Yabai")
    hs.hotkey.bind({"alt"}, "t", function()
      spoon.Yabai:run({"-m", "window", "--toggle", "float"}, function()
        spoon.Yabai:run({"-m", "window", "--grid", "4:4:1:1:2:2"})
      end)
    end)

    function reloadConfig(files)
      hs.reload()
    end
    myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/init.lua", reloadConfig):start()
    hs.alert.show("Config loaded")
  '';
}
