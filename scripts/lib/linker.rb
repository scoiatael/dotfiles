require_relative 'link'

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

  def links(args)
    args.each { |a| link(a) }
  end
end
