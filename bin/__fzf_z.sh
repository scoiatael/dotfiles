#!/usr/bin/env elvish

use re

# https://github.com/zzamboni/elvish-themes/blob/master/chain.elv#L195
fn -prompt-pwd {
  var tmp = (tilde-abbr $pwd)
  re:replace '(\.?[^/]{3})[^/]*/' '$1/' $tmp
}

var DIR = (
    zoxide query -l |
        fzf-tmux-popup --preview 'echo {}; exa {}' --layout=reverse --preview-window "top:3:wrap"
)

cd $DIR

tmux rename-window (-prompt-pwd)
exec (get-env SHELL)
