---
name: flake-inputs
description: Resolve a flake input to its store path with ,flake-input, so its source can be read directly. Use when you need to check what an input actually provides — a home-manager module's options, a flake's nixConfig, an overlay's definitions — rather than guessing from docs or memory.
---

# Reading flake input sources

`bin/,flake-input` prints the store path of a locked flake input:

```
,flake-input llm-agents
,flake-input llm-agents/nixpkgs      # nested input
,flake-input                          # list available inputs
```

It resolves through `nix flake archive --dry-run`, so it reports what the
lockfile actually pins and needs no `--impure` or `builtins.getFlake`.

## Why this beats guessing

The pinned input is the ground truth. Option names, defaults, and assertions
drift between releases, so read the module rather than recalling it:

```
bat "$(,flake-input home-manager)/modules/programs/claude-code.nix"
rg -l 'mkEnableOption' "$(,flake-input home-manager)/modules/programs" | head
```

Checking whether a flake ships a binary cache — its `nixConfig` is ignored
unless the local config trusts it:

```
rg -A6 nixConfig "$(,flake-input llm-agents)/flake.nix"
```

Listing what an input exposes without a network round-trip:

```
nix eval --json "$(,flake-input llm-agents)#packages.aarch64-darwin" \
  --apply 'builtins.attrNames'
```

## Notes

Add `--flake PATH` to target a flake other than `$DOTFILES`.

The path is a read-only store copy of the input's source, so grep and read it
freely — nothing there can be edited.
