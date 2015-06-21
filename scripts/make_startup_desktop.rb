#!/usr/bin/env ruby

require 'fileutils'

STARTUP_SCRIPT = File.expand_path(File.dirname(__FILE__) + '/../scripts/startup.sh')
CONFIG_AUTOSTART = ENV['HOME'] + '/.config/autostart'
STARTUP_DESKTOP = CONFIG_AUTOSTART + '/startup.desktop'

STARTUP_ENTRY = <<EOF
[Desktop Entry]
Name=Utilities autostart
Exec=#{STARTUP_SCRIPT}
Icon=rxvt-unicode
Type=Application
Categories=Utility
EOF

FileUtils.mkpath(CONFIG_AUTOSTART)

File.open(STARTUP_DESKTOP, 'w') do |f|
  f << STARTUP_ENTRY
end
