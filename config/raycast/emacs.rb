#!/usr/bin/env ruby

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Workspaces:
# @raycast.mode inline
#
# Optional parameters:
# @raycast.packageName Emacs
# @raycast.icon images/emacs.png
#
# Conditional parameters:
# @raycast.refreshTime 5m

RED = 31
GREEN = 32
YELLOW = 33

projects = `$HOME/.nix-profile/bin/emacsclient -e "(+workspace-list-names)"`[1..-1].split

if projects.length == 0
  message = "None"
  puts "\e[#{RED}m#{message}\e[0m"
else
  color = (projects.length <= 4) ? GREEN : YELLOW
  message = "#{projects.length} currently open"
  puts "\e[#{color}m#{message}\e[0m"
end
