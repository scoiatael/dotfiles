#!/usr/bin/env ruby

require 'rbconfig'
require_relative 'lib/linker'

# rubocop:disable Metrics/BlockLength
Linker.create! do
  link 'config/ranger', to: 'ranger'
  link 'config/fish', to: 'fish'

  link 'profile', to: 'bash/profile'
  link 'bashrc', to: 'bash/profile'
  link 'direnvrc', to: 'bash/direnvrc'
  touch 'envrc'

  group 'git' do
    link 'gitconfig'
    link 'gitignore_global', to: "#{current_group}/gitignore"
  end

  group 'node' do
    link 'local/package.json', to: "#{current_group}/package.json"
  end

  group 'misc' do
    links %w(dir_colors curlrc slate)

    link 'lein/profiles.clj', to: "#{current_group}/lein_profiles.clj"

    link 'config/terminator/config', to: "#{current_group}/terminator"
  end

  group 'haskell' do
    link 'xmonad/xmonad.hs', to: "#{current_group}/xmonad.hs"
    link 'stack/config.yml', to: "#{current_group}/stack.yml"
  end

  group 'emacs' do
    link 'emacs.d', to: "#{current_group}/spacemacs.d"
    link 'spacemacs'
  end

  group 'ssh' do
    link 'ssh/config', to: "#{current_group}/config"
  end

  group 'vim' do
    link 'config/nvim', to: "#{current_group}/SpaceVim"
    link 'vim', to: "#{current_group}/SpaceVim"

    link 'SpaceVim.d', to: "#{current_group}/SpaceVim.d"
  end

  group 'gnupg' do
    link 'gnupg/gpg.conf', to: "#{current_group}/gpg.conf"
    case RbConfig::CONFIG['host_os']
    when /linux/
      link 'gnupg/gpg-agent.conf', to: "#{current_group}/gpg-agent.linux.conf"
    when /darwin/
      link 'gnupg/gpg-agent.conf', to: "#{current_group}/gpg-agent.darwin.conf"
    end
  end
end
