# Archived Flake Inputs

This file documents flake inputs that were removed from flake.nix but may be useful for future reference.

## Removed on 2026-03-12

### lix & lix-module
- https://lix.systems/add-to-config/#using-the-lix-nixos-module-only-recommended-for-bleeding-edge-builds

        inputs.lix = {
            url = "https://git.lix.systems/lix-project/lix/archive/main.tar.gz";
            flake = false;
        };
        inputs.lix-module = {
            url = "https://git.lix.systems/lix-project/nixos-module/archive/main.tar.gz";
            inputs.nixpkgs.follows = "nixpkgs";
            inputs.lix.follows = "lix";
        };
    
- lix build was failing

        error: builder for '/nix/store/pkpwsq524f4862jvdnl7n66cg3czqckf-lix-2.95.0-pre20260131-dev_64d610f.drv' failed with exit code 1;
            last 25 log lines:
            > [==========] 2134 tests from 77 test suites ran. (175 ms total)
            > [  PASSED  ] 2133 tests.
            > [  FAILED  ] 1 test, listed below:
            > [  FAILED  ] guessOrInventPath.sockets
            >
            >  1 FAILED TEST
            >   YOU HAVE 2 DISABLED TESTS
            >
            > 
            > stderr:
            > Using configuration: seed=16492530101672097640
            > error: executing '/nix/store/q183pd2icip67wjqv6cg39kz9cg5q95d-lix-2.95.0-pre20260131-dev_64d610f/libexec/lix/unix-bind-connect': No such file or directory
            >
            >
            > 6/6 lix:check / libexpr-unit-tests         OK              2.15s
            >
            > Summary of Failures:
            >
            > 5/6 lix:check / libutil-unit-tests  FAIL            2.12s   exit status 1
            >
            > Ok:                4
            > Fail:              1
            > Skipped:           1

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
