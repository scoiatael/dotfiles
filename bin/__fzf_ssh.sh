#!/usr/bin/env bash

HOST=$(
    awk '{ split($1, A, ","); print(A[1]) }' <"$HOME/.ssh/known_hosts" |
        fzf-tmux-popup \
            --preview 'host {}' \
            --layout=reverse \
            --preview-window "top:3:wrap"
)

test -z "$HOST" && (
    echo "No host specified"
    sleep 1
    exit 1
)

tmux rename-window $HOST
ssh "$HOST" || sleep 60
