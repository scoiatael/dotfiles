#!/usr/bin/env ruby

BASE = 'https://cht.sh'
lang = ARGV[0]
args = ARGV[1..-1].join('+')
url = "#{BASE}/#{lang}/#{args}"

exec('curl', url)
