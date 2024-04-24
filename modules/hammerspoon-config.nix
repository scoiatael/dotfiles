{ config, lib, pkgs, ... }:

{
  home.file.".hammerspoon/Spoons" = {
    source = ../config/Spoons;
    recursive = true;
  };
  # TODO: Port more stuff from https://github.com/rtauziac/Hammerspoon-Yabai/blob/master/.hammerspoon/init.lua
  # TODO: Port more stuff from https://github.com/treffynnon/nix-setup/blob/master/home-configs/hammerspoon/init.lua
  # TODO: https://github.com/agzam/spacehammer
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

    switcher_term = hs.window.switcher.new{'Alacritty'}
    hs.hotkey.bind(super, "t", function()switcher_term:next()end)

    switcher_editor = hs.window.switcher.new{'Emacs', 'VSCodium'}
    hs.hotkey.bind(super, "e", function()switcher_editor:next()end)

    switcher_browser = hs.window.switcher.new{'Arc', 'Safari'}
    hs.hotkey.bind(super, "b", function()switcher_browser:next()end)

    hs.hotkey.bind("alt", ",", function()
      spoon.WindowSpace:move("left", true)
    end)
    hs.hotkey.bind("alt", ".", function()
      spoon.WindowSpace:move("right", true)
    end)

    hs.hotkey.bind(super, "left", function()
      spoon.Yabai:run({"-m", "window", "--focus", "west"})
    end)
    hs.hotkey.bind(super, "right", function()
      spoon.Yabai:run({"-m", "window", "--focus", "east"})
    end)
    hs.hotkey.bind(super, "up", function()
      spoon.Yabai:run({"-m", "window", "--focus", "north"})
    end)
    hs.hotkey.bind(super, "down", function()
      spoon.Yabai:run({"-m", "window", "--focus", "south"})
    end)
    hs.hotkey.bind(super, "\\", function()
      hs.execute("touch ~/.config/rio/sentinel")
    end)

    hs.hotkey.bind(super, "c", function()
      hs.execute("pkill yabai")
      reloadConfig()
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

    local myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/init.lua", reloadConfig):start()
    hs.alert.closeSpecific(toastUuid)
    hs.alert.show("Config loaded")
  '';
}

# Local Variables:
# compile-command: "home-manager switch --flake 'path:/Users/lucasczaplinski/dotfiles'"
# End:
