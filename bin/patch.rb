#!/usr/bin/env ruby
require 'set'

# Usage: npx pyright . | patch.rb '# type: ignore'
TXT = ARGV[0]

FILES = $stdin.read.lines

WRITTEN = Set.new

def patch(fileline)
  file, line, _ = fileline.strip.split(":")

  key = [file,line]
  return if WRITTEN.include?(key)
  WRITTEN << key

  puts("M: #{file}:#{line}")
  by_line = File.read(file).lines
  puts("=> #{by_line.size}")
  idx = line.to_i - 1
  by_line[idx].rstrip!
  by_line[idx] << (" " + TXT + "\n")
  File.write(file, by_line.join(""))
end

FILES.map(&method(:patch))
