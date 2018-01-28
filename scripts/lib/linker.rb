require_relative 'link'
require 'fileutils'
require 'rbconfig'

class Linker
  def self.create!(&block)
    new(&block).create!
  end

  attr_reader :block, :current_group

  def initialize(&block)
    fail ArgumentError, 'Block expected' if block.nil?
    @block = block
  end

  def create!
    instance_eval(&block)
  end

  def group(group, &block)
    @current_group = group
    instance_eval(&block)
  end

  def link(dest, to: nil)
    Link.new(dest, to, current_group).create!
  end

  def touch(dest)
      Link.new(dest).touch!
  end

  def links(args)
    args.each { |a| link(a) }
  end

  OS = %w{linux darwin}
  OS.each do |os|
    define_method("#{os}?") do
      RbConfig::CONFIG['host_os'] =~ /#{os}/
    end
  end

  def os
    OS.each do |os|
      return os if send("#{os}?")
    end
  end
end
