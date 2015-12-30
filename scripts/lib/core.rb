require 'fileutils'
require 'pathname'

CONFIG_DIR = Pathname
             .new(__FILE__) # /home/luki/dotfiles/scripts/lib/core.rb
             .dirname       # /home/luki/dotfiles/scripts/lib
             .parent        # /home/luki/dotfiles/scripts/
             .parent        # /home/luki/dotfiles/
             .expand_path

def read_char
  state = `stty -g`
  `stty raw -echo -icanon isig`

  STDIN.getc.chr
ensure
  `stty #{state}`
end

def ask_if(q)
  puts "#{q}? [y/N]"
  read_char.downcase == 'y'
end

def exist?(name)
  File.exist?(name) || File.symlink?(name)
end
