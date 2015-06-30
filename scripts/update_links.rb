#!/usr/bin/env ruby

require 'fileutils'

def get_char
  state = `stty -g`
  `stty raw -echo -icanon isig`

  STDIN.getc.chr
ensure
  `stty #{state}`
end

def ask_if(q)
  puts "#{q}? [y/N]"
  get_char.downcase == 'y'
end

def exist?(name)
  File.exist?(name) || File.symlink?(name)
end

CONFIG_DIR = File.expand_path(File.dirname(__FILE__) << '/..')
LINK_PATH = CONFIG_DIR + '/misc/links.txt'
File.open(LINK_PATH) do |f|
  f.each_line do |line|
    next if line =~ /^#/ # Comment

    to, from = line.split('->').map(&:strip)
    to = "#{ENV['HOME']}/.#{to}"
    from = "#{CONFIG_DIR}/#{from}"

    fail ArgumentError, "File missing: #{from}" unless exist?(from)

    if exist?(to)
      next if File.identical?(from, to)

      puts `ls -l #{to}`
      if ask_if('remove')
        File.delete(to)
      else
        File.rename(to, "#{to}.bak") if ask_if('rename')
      end
    end

    next if exist?(to)

    dirname = File.dirname(to)
    FileUtils.mkpath(dirname) unless File.exist?(dirname)

    puts "Symlinking #{line}"
    File.symlink(from, to)
  end
end
