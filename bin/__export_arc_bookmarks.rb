#!/usr/bin/env ruby

require 'json'
require 'securerandom'

loc = File.expand_path "~/Library/Application Support/Arc/StorableSidebar.json"

def uuid
  SecureRandom.uuid
end

def parse_container_items(f)
  f.dig('sidebar', 'containers', 1, "items").map { next unless it.respond_to?(:dig); [it.dig("data", "tab", "savedTitle"), it.dig("data", "tab", "savedURL")].compact }.reject(&:nil?).reject(&:empty?)
end

fs = JSON.parse File.read(loc)
parse_container_items(fs).each do |(title, url)| 
  puts <<~EOF
      "#{title}" = {
        id = "#{uuid}";
        workspace = spaces.default.id;
        url =  "#{url}";
        isEssential = false;
      };
  EOF
end
