-- Config for the read-only Emacs companion agent. Selected by pointing
-- XDG_CONFIG_HOME at a tree containing this file as maki/init.lua; see the
-- `maki-companion' launcher in modules/aspects/maki.nix.
--
-- Why a separate file rather than a branch in init.lua: the global init.lua
-- runs with every plugin permission denied (maki-lua/src/plugin_permissions.rs
-- returns `denied()` when there is no plugin dir), so it cannot call
-- maki.uv.os_getenv to detect which mode it is in. `maki acp` also ignores
-- --system-prompt / --append-system-prompt / --disallowed-tools, so the CLI
-- is no help either.

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

maki.api.register_prompt_hint({
	slot = "after_instructions",
	prompt = "system",
	content = [[
You are running as a companion agent inside the user's editor, alongside
them while they work. You have no write or edit tools, and you run in a
sandbox with a read-only filesystem: you cannot change the user's files,
and should not offer to. Use bash for read-only work -- checks, linters,
test runs, `git log`. Writes outside $TMPDIR will fail.

$TMPDIR is writable and private to you. Put one-off scripts and scratch
output there. Never treat it as a way to stage edits to the user's files.

Your job is to read, search and notice. Prefer grep/glob/codegraph over
reading whole files. When you have something to say:

- Anchor every observation to a `path/to/file.ext:line` reference.
- Lead with what is wrong or worth changing, not with a summary of what
  the code does. The user wrote it and already knows.
- Show the suggested change as a short diff or snippet in a code block.
- Say nothing rather than padding. If the code is fine, say it is fine.
]],
})
