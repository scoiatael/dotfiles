local wezterm = require("wezterm")

-- Equivalent to POSIX basename(3)
-- Given "/foo/bar" returns "bar"
-- Given "c:\\foo\\bar" returns "bar"
function basename(s)
	return string.gsub(s, "(.*[/\\])(.*)", "%2")
end

wezterm.on("format-tab-title", function(tab, tabs, panes, config, hover, max_width)
	local pane = tab.active_pane
	local cwd = pane.get_current_working_dir()
	local title = basename(pane.foreground_process_name) .. " " .. pane.pane_id .. " " .. cwd
	local color = "navy"
	if tab.is_active then
		color = "blue"
	end
	return {
		{ Background = { Color = color } },
		{ Text = " " .. title .. " " },
	}
end)

local mux = wezterm.mux
local act = wezterm.action

local super = "CTRL|ALT"
local ctrl = "CTRL"
local cmd = "CMD"
local hyper = "CMD|CTRL"

local leader = { key = " ", mods = ctrl, timeout_milliseconds = 2000 }
local keys = {
	{ key = "[", mods = cmd, action = act.ActivateTabRelative(-1) },
	{ key = "]", mods = cmd, action = act.ActivateTabRelative(1) },
	-- { key = 'j', mods = 'CMD', action = act.ActivatePaneDirection 'Down', }
	-- { key = 'k', mods = 'CMD', action = act.ActivatePaneDirection 'Up', }
	{ key = "Enter", mods = cmd, action = act.ActivateCopyMode },
	{ key = "R", mods = hyper, action = act.ReloadConfiguration },
	{ key = "+", mods = hyper, action = act.IncreaseFontSize },
	{ key = "-", mods = hyper, action = act.DecreaseFontSize },
	{ key = "0", mods = hyper, action = act.ResetFontSize },
	{ key = "C", mods = cmd, action = act.CopyTo("Clipboard") },
	{ key = "N", mods = cmd, action = act.SpawnWindow },
	{
		key = "U",
		mods = cmd,
		action = act.CharSelect({ copy_on_select = true, copy_to = "ClipboardAndPrimarySelection" }),
	},
	{ key = "v", mods = cmd, action = act.PasteFrom("Clipboard") },
	{ key = "LeftArrow", mods = super, action = act.ActivatePaneDirection("Left") },
	{ key = "RightArrow", mods = super, action = act.ActivatePaneDirection("Right") },
	{ key = "UpArrow", mods = super, action = act.ActivatePaneDirection("Up") },
	{ key = "DownArrow", mods = super, action = act.ActivatePaneDirection("Down") },
	-- { key = 'f', mods = 'CMD', action = act.SplitVertical { domain = 'CurrentPaneDomain' }, }
	-- { key = 'd', mods = 'CMD', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' }, }
	-- { key = 'h', mods = 'CMD', action = act.ActivatePaneDirection 'Left', }
	-- { key = 'l', mods = 'CMD', action = act.ActivatePaneDirection 'Right', }
	{ key = "t", mods = cmd, action = act.SpawnTab("CurrentPaneDomain") },
	{ key = "w", mods = cmd, action = act.CloseCurrentTab({ confirm = true }) },
	{ key = "q", mods = cmd, action = act.CloseCurrentPane({ confirm = true }) },
	-- { key = 'b', mods = 'LEADER|CTRL', action = act.SendString '\x02', }
	-- { key = 'Enter', mods = 'LEADER', action = act.ActivateCopyMode, }
	-- { key = 'p', mods = 'LEADER', action = act.PastePrimarySelection, }
	-- { key = 'k', mods = 'CTRL|ALT', action = act.Multiple
	--   {
	--     act.ClearScrollback 'ScrollbackAndViewport'
	--     act.SendKey { key = 'L', mods = 'CTRL' }
	--   }
	-- }
	-- { key = 'r', mods = 'LEADER', action = act.ActivateKeyTable { name = 'resize_pane', one_shot = false, }, }
}

return {
	-- ...your existing config
	color_scheme = "Catppuccin Mocha", -- or Macchiato, Frappe, Latte
	font = wezterm.font("JetBrainsMono Nerd Font"),
	font_size = 12,
	line_height = 1.2,
	use_dead_keys = false,
	scrollback_lines = 1024 * 1024,
	adjust_window_size_when_changing_font_size = false,
	hide_tab_bar_if_only_one_tab = false, -- doesn't play nicely with INTEGRATED_BUTTONS
	keys = keys,
	leader = leader,
	window_decorations = "INTEGRATED_BUTTONS|RESIZE",
	window_background_opacity = 0.9,
	colors = {
		tab_bar = {
			background = "rgba(0,0,0,0)",
		},
	},
}
