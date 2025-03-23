#!/usr/bin/env sh
# https://baty.net/posts/2025/03/i-went-a-little-nuts-with-remember-mode-in-emacs/

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Remember
# @raycast.mode silent

# Optional parameters:
# @raycast.icon images/remember.png
# @raycast.argument1 { "type": "text", "placeholder": "what" }

# Documentation:
# @raycast.description Open Emacs remember in new frame
# @raycast.author Jack Baty
# @raycast.authorURL https://baty.net

$HOME/.nix-profile/bin/emacsclient -e "(remember-other-frame \"$1\")"
