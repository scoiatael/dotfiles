#!/usr/bin/env ruby

require_relative 'lib/linker'

# rubocop:disable Metrics/BlockLength
Linker.create! do
  link 'config/ranger', to: 'ranger'
  group 'fish' do
    %w[config.fish fishfile].each do |file|
      link "config/fish/#{file}", to: "#{current_group}/#{file}"
    end
  end

  link 'profile', to: 'bash/profile'
  link 'bashrc', to: 'bash/profile'
  link 'direnvrc', to: 'bash/direnvrc'
  touch 'envrc'

  group 'vim' do
    link 'vimrc'
    link 'config/nvim/init.vim', to: "#{current_group}/vimrc"
    link 'config/nvim/bundle', to: "#{current_group}/bundle"
    link 'vim/bundle', to: "#{current_group}/bundle"
  end

  group 'git' do
    link 'gitconfig'
    link 'gitconfig_custom', to: "#{current_group}/gitconfig_custom_#{os}"
    link 'gitignore_global', to: "#{current_group}/gitignore"
  end

  group 'node' do
    link 'local/package.json', to: "#{current_group}/package.json"
  end

  group 'misc' do
    links %w(dir_colors curlrc)

    link 'lein/profiles.clj', to: "#{current_group}/lein_profiles.clj"
  end

  group 'haskell' do
    link 'stack/config.yml', to: "#{current_group}/stack.yml"
  end

  group 'emacs' do
    link 'emacs.d', to: "#{current_group}/spacemacs.d"
    link 'spacemacs'
  end

  group 'ssh' do
    link 'ssh/config', to: "#{current_group}/config"
  end

  group 'gnupg' do
    link 'gnupg/gpg.conf', to: "#{current_group}/gpg.conf"
    link 'gnupg/gpg-agent.conf', to: "#{current_group}/gpg-agent.#{os}.conf"
  end

  group 'macos' do
    links %w(skhdrc chunkwmrc) if darwin?
  end
end
