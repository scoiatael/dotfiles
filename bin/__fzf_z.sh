#!/usr/bin/env bash

DIR=$(
    zoxide query |
        fzf-tmux-popup \
            --preview 'exa -ld --git {}' \
            --preview-window "bottom:1:wrap"
)

cd "$DIR" || exit 1
tmux rename-window $DIR
exec $SHELL
