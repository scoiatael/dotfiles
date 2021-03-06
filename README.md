## My configuration files.

Inspired by [Using GNU Stow to manage your dotfiles](http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html).

## Get stow

### macOS

    brew install stow

## Install everything...

    bash update.sh

## ...or decide which groups to install

### Bash

    stow --dotfiles --adopt bash

Configuration for Bash with Direnv.

### Fish

    mkdir -p ~/.config/fish
    cd fish && stow --dotfiles --adopt .

### Alacritty

    mkdir -p ~/.config/alacritty
    cd alacritty && stow --dotfiles --adopt .

### Emacs

    mkdir -p ~/.config/
    cd emacs && stow --dotfiles --adopt .
    cd emacs-doom && stow --dotfiles --adopt .

### Git

    cd git && stow --dotfiles --adopt .

### Tmux

    cd tmux && stow --dotfiles --adopt .

### vim

    mkdir -p ~/.vim/bundle
    cd vim && stow --dotfiles --adopt .
    cd vim-bundle && stow --dotfiles --adopt .

## Experimental

(installed separately)

#### Ion

    mkdir -p ~/.config/ion
    cd experimental/ion && stow --dotfiles --adopt .

## Platform-specific

(installed separately)

#### MacOS

    cd platform-specific/macos && stow --dotfiles --adopt .
