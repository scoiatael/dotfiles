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
    hs.loadSpoon("WindowSpace")
    hs.loadSpoon('ControlEscape'):start()

    local super = {"ctrl", "alt"}

    local toastUuid
    function reloadConfig()
      toastUuid = hs.alert.show("Reloading config")
      hs.reload()
    end


    hs.hotkey.bind("alt", ",", function()
      spoon.WindowSpace:move("left", true)
    end)
    hs.hotkey.bind("alt", ".", function()
      spoon.WindowSpace:move("right", true)
    end)
    hs.hotkey.bind(super, "h", function()
      spoon.Yabai:run({"-m", "window", "--focus", "west"})
    end)
    hs.hotkey.bind(super, "j", function()
      spoon.Yabai:run({"-m", "window", "--focus", "south"})
    end)
    hs.hotkey.bind(super, "k", function()
      spoon.Yabai:run({"-m", "window", "--focus", "north"})
    end)
    hs.hotkey.bind(super, "l", function()
      spoon.Yabai:run({"-m", "window", "--focus", "east"})
    end)
    hs.hotkey.bind({"alt"}, "t", function()
      spoon.Yabai:run({"-m", "window", "--toggle", "float"}, function()
        spoon.Yabai:run({"-m", "window", "--grid", "4:4:1:1:2:2"})
      end)
    end)
    hs.hotkey.bind(super, "-", function()
      spoon.Yabai:run({"-m", "config", "top_padding", "36"})
    end)
    hs.hotkey.bind(super, "=", function()
      spoon.Yabai:run({"-m", "config", "top_padding", "0"})
    end)
    hs.hotkey.bind(super, "c", reloadConfig)

    local myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/init.lua", reloadConfig):start()
    hs.alert.closeSpecific(toastUuid)
    hs.alert.show("Config loaded")
  '';
}

# Local Variables:
# compile-command: "home-manager switch --flake 'path:/Users/lucasczaplinski/dotfiles'"
# End:
