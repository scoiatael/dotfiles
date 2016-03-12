require_relative 'lib/link'

LOCAL = ENV['HOME'] + '/.local'
LOCAL_BIN = LOCAL + '/bin'

system("cd #{LOCAL} && npm install")

Dir[LOCAL + '/node_modules/.bin/*'].each do |f|
  src = f
  dest = File.absolute_path(LOCAL_BIN + '/' + File.basename(f))
  Link.new(dest, src).create!
end
