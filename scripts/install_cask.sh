#!/bin/bash
[ -d $HOME/.emacs.d ] && echo '~/.emacs.d already exists!' && exit 1

curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python

rmdir $HOME/.emacs.d

ruby $(dirname $0)/update_links.rb
