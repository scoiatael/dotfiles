-- Config for the read-only Emacs companion agent. Selected by pointing
-- XDG_CONFIG_HOME at a tree containing this file as maki/init.lua; see the
-- `maki-companion' launcher in modules/aspects/companion.nix.
--
-- Why a separate file rather than a branch in init.lua: the global init.lua
-- runs with every plugin permission denied (maki-lua/src/plugin_permissions.rs
-- returns `denied()` when there is no plugin dir), so it cannot call
-- maki.uv.os_getenv to detect which mode it is in. `maki acp` also ignores
-- --system-prompt / --append-system-prompt / --disallowed-tools, so the CLI
-- is no help either.
--
-- The system prompt is not here: companion.nix appends a register_prompt_hint
-- call carrying config/companion/prompt.md, which the claude-agent-acp backend
-- also uses. Edit the prompt there.

maki.setup({
	ui = {
		theme = "catpuccin_frappe",
	},
	provider = {
		default_model = "anthropic/claude-sonnet-4-6",
	},
	plugins = {
		websearch = { enabled = false },
		-- Disabling the plugin is the only way to drop a tool outright:
		-- permissions.toml can't gate tools that declare no scopes.
		-- `edit` covers multiedit/edit_lines/insert_lines too. `bash` stays
		-- enabled on purpose (checks, linters, git log) -- the nono
		-- `maki-companion` profile is what stops it writing.
		write = { enabled = false },
		edit = { enabled = false },
	},
})
