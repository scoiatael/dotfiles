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

      puts `ls -l #{to}`
      if ask_if('remove')
        File.delete(to)
      else
        File.rename(to, "#{to}.bak") if ask_if('rename')
      end
    end

    return if exist?(to)

    dirname = File.dirname(to)
    FileUtils.mkpath(dirname) unless File.exist?(dirname)

    puts "Symlinking #{to} -> #{from}"
    File.symlink(from, to)
  end
end
