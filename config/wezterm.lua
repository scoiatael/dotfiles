local wezterm = require("wezterm")

-- Equivalent to POSIX basename(3)
-- Given "/foo/bar" returns "bar"
-- Given "c:\\foo\\bar" returns "bar"
function basename(s)
	return string.gsub(s, "(.*[/\\])(.*)", "%2")
end

-- src: https://github.com/KyleKing/dotfiles/blob/main/dot_wezterm.lua
-- ============================================================================
-- Configuration for Tab Color

-- Based on: https://github.com/protiumx/.dotfiles/blob/854d4b159a0a0512dc24cbc840af467ac84085f8/stow/wezterm/.config/wezterm/wezterm.lua#L291-L319
local process_icons = {
	["bash"] = wezterm.nerdfonts.cod_terminal_bash,
	["btm"] = wezterm.nerdfonts.mdi_chart_donut_variant,
	["cargo"] = wezterm.nerdfonts.dev_rust,
	["curl"] = wezterm.nerdfonts.mdi_flattr,
	["docker"] = wezterm.nerdfonts.linux_docker,
	["docker-compose"] = wezterm.nerdfonts.linux_docker,
	["gh"] = wezterm.nerdfonts.dev_github_badge,
	["git"] = wezterm.nerdfonts.fa_git,
	["go"] = wezterm.nerdfonts.seti_go,
	["htop"] = wezterm.nerdfonts.mdi_chart_donut_variant,
	["kubectl"] = wezterm.nerdfonts.linux_docker,
	["kuberlr"] = wezterm.nerdfonts.linux_docker,
	["lazydocker"] = wezterm.nerdfonts.linux_docker,
	["lazygit"] = wezterm.nerdfonts.oct_git_compare,
	["lua"] = wezterm.nerdfonts.seti_lua,
	["make"] = wezterm.nerdfonts.seti_makefile,
	["node"] = wezterm.nerdfonts.mdi_hexagon,
	["nvim"] = wezterm.nerdfonts.custom_vim,
	["psql"] = "󱤢",
	["ruby"] = wezterm.nerdfonts.cod_ruby,
	["stern"] = wezterm.nerdfonts.linux_docker,
	["sudo"] = wezterm.nerdfonts.fa_hashtag,
	["usql"] = "󱤢",
	["vim"] = wezterm.nerdfonts.dev_vim,
	["wget"] = wezterm.nerdfonts.mdi_arrow_down_box,
	["zsh"] = wezterm.nerdfonts.dev_terminal,
}

-- Return the Tab's current working directory
local function get_cwd(tab)
	-- Note, returns URL Object: https://wezfurlong.org/wezterm/config/lua/pane/get_current_working_dir.html
	if not tab.active_pane.current_working_dir then
		return ""
	end
	return tab.active_pane.current_working_dir.file_path or ""
end

-- Remove all path components and return only the last value
local function remove_abs_path(path)
	return path:gsub("(.*[/\\])(.*)", "%2")
end

-- Return the pretty path of the tab's current working directory
local function get_display_cwd(tab)
	local current_dir = get_cwd(tab)
	return string.gsub(current_dir, os.getenv("HOME"), "~")
end

-- Return the concise name or icon of the running process for display
local function get_process(tab)
	if not tab.active_pane or tab.active_pane.foreground_process_name == "" then
		return "[?]"
	end

	local process_name = remove_abs_path(tab.active_pane.foreground_process_name)
	if process_name:find("kubectl") then
		process_name = "kubectl"
	end

	return process_icons[process_name] or string.format("[%s]", process_name)
end

-- Pretty format the tab title
local function format_title(tab)
	local cwd = get_display_cwd(tab)
	local process = get_process(tab)

	local active_title = tab.active_pane.title
	if active_title:find("- NVIM") then
		active_title = active_title:gsub("^([^ ]+) .*", "%1")
	end

	local description = (not active_title or active_title == cwd) and "~" or active_title
	return string.format(" %s %s ", process, cwd)
end

-- Determine if a tab has unseen output since last visited
local function has_unseen_output(tab)
	if not tab.is_active then
		for _, pane in ipairs(tab.panes) do
			if pane.has_unseen_output then
				return true
			end
		end
	end
	return false
end

-- Returns manually set title (from `tab:set_title()` or `wezterm cli set-tab-title`) or creates a new one
local function get_tab_title(tab)
	local title = tab.tab_title
	-- if the tab title is explicitly set, take that
	if title and #title > 0 then
		return title
	end
	return format_title(tab)
end

-- Convert arbitrary strings to a unique hex color value
-- Based on: https://stackoverflow.com/a/3426956/3219667
local function string_to_color(str)
	-- Convert the string to a unique integer
	local hash = 0
	for i = 1, #str do
		-- Bitwise Left Shift: https://stackoverflow.com/a/141873/3219667
		hash = string.byte(str, i) + ((hash << 5) - hash)
	end
	-- Convert the integer to a unique color
	local c = string.format("%06X", (hash & 0x00FFFFFF))
	return "#" .. (string.rep("0", 6 - #c) .. c):upper()
end

local function select_contrasting_fg_color(hex_color, modifier)
	modifier = modifier or "none"
	local color = wezterm.color.parse(hex_color)
	---@diagnostic disable-next-line: unused-local
	local lightness, _a, _b, _alpha = color:laba()
	if lightness > 55 then
		-- Reversed logic: "lighten" for dark colours should be the most visible
		if modifier == "lighten" then
			return "#000000"
		elseif modifier == "darken" then
			return "#444444"
		else
			return "#222222"
		end
	end
	if modifier == "lighten" then
		return "#FFFFFF"
	elseif modifier == "darken" then
		return "#BBBBBB"
	else
		return "#DDDDDD"
	end
end

local function darken(hex_color, amount)
	local color = wezterm.color.parse(hex_color)
	local r, g, b, a = color:darken(amount):srgba_u8()
	return string.format("#%X%X%X", r, g, b)
end

local function lighten(hex_color, amount)
	local color = wezterm.color.parse(hex_color)
	local r, g, b, a = color:lighten(amount):srgba_u8()
	return string.format("#%X%X%X", r, g, b)
end

-- Inline tests
local testColor = string_to_color("/Users/kyleking/Developer/ProjectA")
assert(testColor == "#EBD168", "Unexpected color value for test hash (" .. testColor .. ")")
assert(select_contrasting_fg_color("#494CED") == "#DDDDDD", "Expected higher contrast with white")
assert(select_contrasting_fg_color("#128b26") == "#DDDDDD", "Expected higher contrast with white")
assert(select_contrasting_fg_color("#58f5a6") == "#222222", "Expected higher contrast with black")
assert(select_contrasting_fg_color("#EBD168") == "#222222", "Expected higher contrast with black")

-- On format tab title events, override the default handling to return a custom title
-- Docs: https://wezfurlong.org/wezterm/config/lua/window-events/format-tab-title.html
---@diagnostic disable-next-line: unused-local
wezterm.on("format-tab-title", function(tab, _tabs, _panes, _config, _hover, _max_width)
	local title = get_tab_title(tab)
	local color = string_to_color(get_cwd(tab))

	if tab.is_active then
		return {
			{ Background = { Color = color } },
			{ Foreground = { Color = select_contrasting_fg_color(color) } },
			{ Text = title },
		}
	end
	if has_unseen_output(tab) then
		return {
			{ Foreground = { Color = color } },
			{ Foreground = { Color = select_contrasting_fg_color(color, "ligthen") } },
			{ Text = title },
		}
	end
	return {
		{ Background = { Color = color } },
		{ Foreground = { Color = select_contrasting_fg_color(color, "darken") } },
		{ Text = title },
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
	tab_max_width = 128,
}
