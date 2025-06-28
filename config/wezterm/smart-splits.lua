local smart_splits = wezterm.plugin.require("https://github.com/mrjones2014/smart-splits.nvim")
-- you can put the rest of your Wezterm config here
smart_splits.apply_to_config(config, {
	direction_keys = { "LeftArrow", "DownArrow", "UpArrow", "RightArrow" },
	modifiers = {
		move = "ALT",
		resize = "CTRL|ALT",
	},
	log_level = "info",
})
