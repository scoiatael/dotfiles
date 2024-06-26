hs.loadSpoon("Yabai")
hs.loadSpoon("WindowSpace")
hs.loadSpoon("ControlEscape"):start()

old_path = package.path
stackline = require("stackline")
stackline:init({
	paths = {
		yabai = "/run/current-system/sw/bin/yabai",
	},
})

-- HACK: Since we do extensive path manipulation, Lua will try to be lazy and cache requires. This breaks stuff.
package.loaded["lib.utils"] = nil
fennel = require("fennel")
package.path = hs.configdir .. "/spacehammer/?.lua;" .. old_path
fennel.path = hs.configdir .. "/spacehammer/?/init.fnl;" .. hs.configdir .. "/spacehammer/?.fnl;" .. old_path
fennel["macro-path"] = hs.configdir .. "/spacehammer/?/macros.fnl;" .. hs.configdir .. "/spacehammer/?.fnl;" .. old_path
require("spacehammer")

local super = { "ctrl", "alt" }
local ctrl = { "ctrl" }
local cmd = { "cmd" }
local hyper = { "cmd", "ctrl" }

local fn = function()
	hs.eventtap.keyStroke({}, "left", 100)
end
hs.hotkey.bind(ctrl, "h", fn, nil, fn)
local fn = function()
	hs.eventtap.keyStroke({}, "down", 100)
end
hs.hotkey.bind(ctrl, "j", fn, nil, fn)
local fn = function()
	hs.eventtap.keyStroke({}, "up", 100)
end
hs.hotkey.bind(ctrl, "k", fn, nil, fn)
local fn = function()
	hs.eventtap.keyStroke({}, "right", 100)
end
hs.hotkey.bind(ctrl, "l", fn, nil, fn)

hs.hotkey.bind("alt", ",", function()
	spoon.Yabai:run({ "-m", "window", "--focus", "stack.prev" })
end)
hs.hotkey.bind("alt", ".", function()
	spoon.Yabai:run({ "-m", "window", "--focus", "stack.next" })
end)

hs.hotkey.bind(hyper, "h", function()
	spoon.Yabai:run({ "-m", "window", "--focus", "west" })
end)
hs.hotkey.bind(hyper, "l", function()
	spoon.Yabai:run({ "-m", "window", "--focus", "east" })
end)
hs.hotkey.bind(hyper, "k", function()
	spoon.Yabai:run({ "-m", "window", "--focus", "north" })
end)
hs.hotkey.bind(hyper, "j", function()
	spoon.Yabai:run({ "-m", "window", "--focus", "south" })
end)

hs.hotkey.bind(super, "c", function()
	hs.execute("pkill yabai")
	reloadConfig()
end)
hs.hotkey.bind(super, "\\", function()
	stackline.config:toggle("appearance.showIcons")
end)

hs.hotkey.bind({ "alt" }, "t", function()
	spoon.Yabai:run({ "-m", "window", "--toggle", "float" }, function()
		spoon.Yabai:run({ "-m", "window", "--grid", "4:4:1:1:2:2" })
	end)
end)

hs.hotkey.bind(super, "-", function()
	spoon.Yabai:run({ "-m", "config", "top_padding", "36" })
end)
hs.hotkey.bind(super, "=", function()
	spoon.Yabai:run({ "-m", "config", "top_padding", "0" })
end)
