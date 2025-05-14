#!/usr/bin/env sh

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Capture
# @raycast.mode silent

# Optional parameters:
# @raycast.icon images/capture.png

$HOME/.nix-profile/bin/emacsclient -e "(+org-capture/open-frame)"

command -v osascript > /dev/null && osascript -e 'tell application "Emacs" to activate'
