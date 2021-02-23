#!/usr/bin/env ruby

require_relative 'lib/linker'

# rubocop:disable Metrics/BlockLength
Linker.create! do
  group 'fish' do
    %w[config.fish].each do |file|
      link "config/fish/#{file}", to: "#{current_group}/#{file}"
    end
  end

  link 'profile', to: 'bash/profile'
  link 'bashrc', to: 'bash/profile'
  link 'direnvrc', to: 'bash/direnvrc'

  link 'config/ion', to: 'experimental/ion'
  link 'config/alacritty/alacritty.yml', to: 'experimental/alacritty.yml'
  link 'tmux.conf', to: 'experimental/tmux/tmux.conf'

  touch 'envrc'

  group 'vim' do
    link 'vimrc'
    link 'config/nvim/init.vim', to: "#{current_group}/vimrc"
    link 'config/nvim/bundle', to: "#{current_group}/bundle"
    link 'vim/bundle', to: "#{current_group}/bundle"
  end

  group 'git' do
    link 'gitconfig'
    link 'gitignore_global', to: "#{current_group}/gitignore"
  end

  group 'misc' do
    links %w(dir_colors curlrc)
  end

  group 'emacs' do
    link 'emacs.d'
    link 'config/doom' , to: "#{current_group}/doom.d"
  end

  group 'ssh' do
    link 'ssh/config', to: "#{current_group}/config"
  end
end
