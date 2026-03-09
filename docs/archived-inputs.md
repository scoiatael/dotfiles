# Archived Flake Inputs

This file documents flake inputs that were removed from flake.nix but may be useful for future reference.

## Removed on 2026-03-09

### fastanime
- **URL**: `github:Benexl/FastAnime`
- **Purpose**: Anime streaming client
- **Reason for removal**: Not actively used
- **To restore**: Check git history around this date

### gitAlias
- **URL**: `github:GitAlias/gitalias`
- **Purpose**: Git alias collection
- **Reason for removal**: Aliases already defined inline in modules/git.nix (lines 24-76)

### rio (terminal emulator)
- **URL**: `github:raphamorim/rio/0.0.x`
- **Purpose**: GPU-accelerated terminal emulator
- **Reason for removal**: Using wezterm instead (modules/wezterm.nix)
- **Related**: catppuccin-rio theme (also removed)

### catppuccin-rio
- **URL**: `github:catppuccin/rio`
- **Purpose**: Catppuccin theme for rio terminal
- **Reason for removal**: rio terminal not in use

### nix-vscode-extensions
- **URL**: `github:nix-community/nix-vscode-extensions`
- **Purpose**: VSCode extension management via Nix
- **Reason for removal**: Not using VSCode integration with Nix

### Talon Voice (talonhub_community, cursorless_talon)
- **URLs**:
  - `github:talonhub/community`
  - `github:cursorless-dev/cursorless-talon`
- **Purpose**: Voice control for coding with Talon Voice
- **Reason for removal**: Not actively using Talon Voice setup

### Hammerspoon configs (agzam_spacehammer, AdamWagner_stackline)
- **URLs**:
  - `github:agzam/spacehammer`
  - `github:AdamWagner/stackline`
- **Purpose**: Alternative Hammerspoon configuration frameworks
- **Reason for removal**: Using custom Hammerspoon config (config/hammerspoon.lua)

## Notes

All these inputs were commented out in flake.nix and not actively used. They are preserved here for historical context and can be restored from git history if needed in the future.
