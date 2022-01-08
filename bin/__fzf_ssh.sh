#!/usr/bin/env elvish

var HOST = (
    cat ~/.ssh/known_hosts |
    awk '{ split($1, A, ","); print(A[1]) }' |
    sort |
    uniq |
    egrep -v '([0-9]{1,3}\.){3}[0-9]{1,3}' |
    fzf-tmux-popup --preview-window "top:3:wrap" --preview 'host {}' --layout=reverse
)

var BASE = (str:split "." $HOST | take 1)
var DOMAIN = (str:split "." $HOST | drop 1 | tac | cut -c 1 | tr "\n")

tmux rename-window $DOMAIN$BASE
if (not ?(ssh $HOST)) {
    sleep 60
}
