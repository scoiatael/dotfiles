#!/usr/bin/env sh

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Paste as Plain Text
# @raycast.mode silent

# Gram pastes nothing when the clipboard also carries public.url (e.g. from Arc).
pbpaste | pbcopy
osascript -e 'tell application "System Events" to keystroke "v" using command down'
