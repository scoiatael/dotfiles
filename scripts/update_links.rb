#!/usr/bin/env ruby

require_relative 'lib/linker'

Linker.create! do
  group 'vim' do
    links %w(vim vimrc)
    %w(pluginrc vundlerc bindrc).each do |file|
      link "#{current_group}-#{file}", to: "#{current_group}/#{file}"
    end
  end

  group 'prezto' do
    link 'zprezto', to: 'prezto'
  end

  group 'rvm' do
    link 'rvmrc'
    link 'rvm/gemsets/default.gems', to: "#{current_group}/default.gems"
  end

  group 'prezto/runcoms' do
    links %w(zprofile zshrc zshenv zpreztorc zlogin zlogout)
  end

  group 'tmux' do
    links %w(tmux tmuxln).map { |f| f + '.conf' }
  end

  group 'git' do
    link 'gitconfig'
    link 'gitignore_global', to: "#{current_group}/gitignore"
  end

  group 'node' do
    link 'package.json', to: 'local/package.json'
  end

  group 'misc' do
    links %w(dir_colors gmrunrc Xdefaults xscreensaver curlrc)

    link 'lein/profiles.clj', to: "#{current_group}/lein_profiles.clj"
    link 'config/redshift.conf', to: "#{current_group}/redshift.conf"
  end

  group 'haskell' do
    link 'xmonad/xmonad.hs', to: "#{current_group}/xmonad.hs"
    link 'stack/config.yml', to: "#{current_group}/stack.yml"
  end

  group 'emacs' do
    link 'emacs.d', to: "#{current_group}/spacemacs.d"
    link 'spacemacs'
  end
end
