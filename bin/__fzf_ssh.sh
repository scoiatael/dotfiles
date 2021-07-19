#!/usr/bin/env bash

HOST=$(
    awk '{ split($1, A, ","); print(A[1]) }' <"$HOME/.ssh/known_hosts" |
        fzf-tmux-popup \
            --preview 'host {}' \
            --preview-window "bottom:1:wrap"
)

tmux rename-window $HOST
exec ssh "$HOST"
