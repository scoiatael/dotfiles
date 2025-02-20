#!/usr/bin/env elvish -norc

use re

# https://github.com/zzamboni/elvish-themes/blob/master/chain.elv#L195
fn -prompt-pwd {
  var tmp = (tilde-abbr $pwd)
  re:replace '(\.?[^/]{3})[^/]*/' '$1/' $tmp
}

var DIR = (
    zoxide query -l |
        fzf-tmux --preview 'echo {}; exa {}' --layout=reverse --preview-window "top:3:wrap"
)

cd $DIR

exec (get-env SHELL)
