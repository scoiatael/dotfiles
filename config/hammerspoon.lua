hs.loadSpoon("WindowSpace")
hs.loadSpoon("ControlEscape"):start()

old_path = package.path

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
