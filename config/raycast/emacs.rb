#!/usr/bin/env ruby

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Emacs:
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

projects = `$HOME/.nix-profile/bin/emacsclient -e "(projectile-open-projects)"`[1..-1].split

if projects.length == 0
  message = "No open projects"
  puts "\e[#{RED}m#{message}\e[0m"
else
  color = (projects.length <= 5) ? GREEN : YELLOW
  message = "You have #{projects.length} open projects"
  puts "\e[#{color}m#{message}\e[0m"
end
