#!/usr/bin/env sh

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title emacs-file
# @raycast.mode silent

# Optional parameters:
# @raycast.icon images/emacs.png
# @raycast.argument1 { "type": "text", "placeholder": "what" }

$HOME/.nix-profile/bin/emacsclient -e "(scoiatael/open-file-url \"$1\")"
