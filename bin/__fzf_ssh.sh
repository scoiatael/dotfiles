#!/usr/bin/env elvish -norc

use re
use str
use github.com/zzamboni/elvish-modules/util

var HOST = (
    cat /Users/opera_user/.ssh/known_hosts |
    each { |x| re:split "[, ]" $x | take 1 } |
    util:select { |x| and (str:contains $x ".") (not (re:match '([0-9]{1,3}\.){3}[0-9]{1,3}' $x)) } |
    order |
    to-lines |
    fzf-tmux-popup --preview-window "top:3:wrap" --preview 'host {}' --layout=reverse
)

var BASE = (str:split "." $HOST | take 1)
var DOMAIN = (str:split "." $HOST | drop 1 | to-lines | tac | cut -c 1 | str:join ".")

tmux rename-window $DOMAIN.$BASE
try { ssh $HOST } except e {
    put $e
    sleep 60
}
