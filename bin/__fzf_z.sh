#!/usr/bin/env bash

DIR=$(
    zoxide query -l |
        fzf-tmux-popup \
            --preview 'exa -ld --git {}' \
            --preview-window "top:3:wrap"
)

cd "$DIR" || exit 1
tmux rename-window $DIR
exec $SHELL
