hs.loadSpoon("Yabai")
hs.loadSpoon("WindowSpace")
hs.loadSpoon("ControlEscape"):start()

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
	spoon.WindowSpace:move("left", true)
end)
hs.hotkey.bind(super, ",", function()
	hs.eventtap.keyStroke({ "ctrl", "fn" }, "left", 1000)
end)
hs.hotkey.bind("alt", ".", function()
	spoon.WindowSpace:move("right", true)
end)
hs.hotkey.bind(super, ".", function()
	hs.eventtap.keyStroke({ "ctrl", "fn" }, "right", 1000)
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
hs.hotkey.bind(super, "\\", function()
	hs.execute("touch ~/.config/rio/sentinel")
end)

hs.hotkey.bind(super, "c", function()
	hs.execute("pkill yabai")
	reloadConfig()
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
