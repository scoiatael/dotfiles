require_relative 'core'

class Link
  attr_reader :to, :from

  def initialize(to, from = nil, group = nil)
    group ||= 'misc'

    @to = to
    @to = "#{ENV['HOME']}/.#{@to}" unless @to.start_with?('/')

    @from = from.nil? ? "#{group}/#{to}" : from
    @from = "#{CONFIG_DIR}/#{@from}" unless @from.start_with?('/')
  end

  def touch!
      FileUtils.touch(@to)
  end

  def create!
    fail ArgumentError, "File missing: #{from}" unless exist?(from)

    if exist?(to)
      return if File.identical?(from, to)
      handle_existing(to)
    end

    return if exist?(to)

    dirname = File.dirname(to)
    if exist?(dirname)
      unless File.directory?(dirname)
        handle_existing(dirname)
        FileUtils.mkpath(dirname)
      end
    else
      FileUtils.mkpath(dirname)
    end

    puts "Symlinking #{to} -> #{from}"
    File.symlink(from, to)
  end

  private

  def handle_existing(to)
    puts `ls -ld #{to}`
    if ask_if('remove')
      return File.delete(to)
    end
    if ask_if('rename')
      File.rename(to, "#{to}.bak")
    end
  end
end
