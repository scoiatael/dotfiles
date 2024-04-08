local obj = {}
obj.__index = obj

obj.logger = hs.logger.new("WindowSpace", "debug")

local hotkey = require("hs.hotkey")
local window = require("hs.window")
local spaces = require("hs.spaces")

function getGoodFocusedWindow(nofull)
	local win = window.focusedWindow()
	if not win or not win:isStandard() then
		return
	end
	if nofull and win:isFullScreen() then
		return
	end
	return win
end

function flashScreen(screen)
	local flash = hs.canvas.new(screen:fullFrame()):appendElements({
		action = "fill",
		fillColor = { alpha = 0.25, red = 1 },
		type = "rectangle",
	})
	flash:show()
	hs.timer.doAfter(0.15, function()
		flash:delete()
	end)
end

function switchSpace(skip, dir)
	for i = 1, skip do
		hs.eventtap.keyStroke({ "ctrl", "fn" }, dir, 0) -- "fn" is a bugfix!
	end
end

function obj:move(dir, switch)
	local win = getGoodFocusedWindow(true)
	if not win then
		return
	end
	local screen = win:screen()
	local uuid = screen:getUUID()
	local userSpaces = nil
	for k, v in pairs(spaces.allSpaces()) do
		userSpaces = v
		if k == uuid then
			break
		end
	end
	if not userSpaces then
		return
	end
	local thisSpace = spaces.windowSpaces(win) -- first space win appears on
	if not thisSpace then
		return
	else
		thisSpace = thisSpace[1]
	end
	local last = nil
	local skipSpaces = 0
	obj.logger.d("spaces", hs.inspect(userSpaces), "<-", thisSpace)
	for _, spc in ipairs(userSpaces) do
		obj.logger.d(spc, spaces.spaceType(spc), dir)
		if spaces.spaceType(spc) ~= "user" then -- skippable space
			skipSpaces = skipSpaces + 1
		else
			if last and ((dir == "left" and spc == thisSpace) or (dir == "right" and last == thisSpace)) then
				local newSpace = (dir == "left" and last or spc)
				if switch then
					-- spaces.gotoSpace(newSpace)  -- also possible, invokes MC
					switchSpace(skipSpaces + 1, dir)
				end
				spaces.moveWindowToSpace(win, newSpace)
				return
			end
			last = spc -- Haven't found it yet...
			skipSpaces = 0
		end
	end
	obj.logger.e("No space found")
	if hs.spaces.addSpaceToScreen(uuid, false) then
		return
	end
	flashScreen(screen) -- Shouldn't get here, so no space found
end

return obj
