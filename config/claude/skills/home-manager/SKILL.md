---
name: home-manager
description: Inspect evaluated home-manager options in the dotfiles flake using the ,hm-eval and ,hm-cat helpers. Use when verifying what a nix module actually generates — a settings file, an MCP config, a wrapper script — instead of rebuilding first or reading the symlinks under ~/.
---

# Inspecting home-manager config

`bin/,hm-eval` and `bin/,hm-cat` evaluate options for a host in the dotfiles
flake without a rebuild. Both default to the current hostname and `$USER`, and
take `--host`, `--user`, `--flake`.

Prefer these over `darwin-rebuild`/`nixos-rebuild` when the question is "what
does this module produce?" — evaluation is cheap and touches nothing.

## ,hm-eval — evaluate an option

```
,hm-eval programs.claude-code.settings
,hm-eval home.packages --apply 'map (p: p.name)'
,hm-eval --host LsAir programs.mcp.servers
```

Trailing arguments pass through to `nix eval`, defaulting to `--json`.

## ,hm-cat — print a generated file

```
,hm-cat 'xdg.configFile."mcp/mcp.json".source'
,hm-cat --path 'home.file."/Users/lukas/.claude/settings.json".source'
```

The option must evaluate to a store path. Reading it realises the derivation,
so there is no separate `nix build` step. `--path` prints the store path
instead of the contents.

## Gotchas

Attribute paths are `nix eval` installables. Quote any segment containing a dot
or a slash:

```
,hm-eval 'xdg.configFile."mcp/mcp.json".source'
```

`home.file` keys are whatever the module used as the attribute name. Modules
that build paths from `config.home.homeDirectory` produce absolute keys —
`home.file."/Users/lukas/.claude/settings.json"`, not `home.file.".claude/..."`.
When a lookup fails with "does not provide attribute", list the real keys:

```
,hm-eval home.file --apply 'builtins.attrNames'
```

The flake is referenced by path, so no `--impure` is needed.

A newly created file is invisible to the flake until it is `git add`ed.
