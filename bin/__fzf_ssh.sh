#!/usr/bin/env bash

HOST=$(
    awk '{ split($1, A, ","); print(A[1]) }' <"$HOME/.ssh/known_hosts" |
        fzf-tmux-popup \
            --preview 'host {}' \
            --layout=reverse \
            --preview-window "top:3:wrap"
)

tmux rename-window $HOST
exec ssh "$HOST"
