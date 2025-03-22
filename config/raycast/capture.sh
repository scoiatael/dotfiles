#!/usr/bin/env sh

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Capture
# @raycast.mode silent

# Optional parameters:
# @raycast.icon ✉︎

$HOME/.nix-profile/bin/emacsclient -e "(+org-capture/open-frame)"
