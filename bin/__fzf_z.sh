#!/usr/bin/env bash

DIR=$(
    zoxide query -l |
        fzf-tmux-popup \
            --preview 'exa -ld --git {}' \
            --preview-window "top:3:wrap"
)

test -z "$DIR" && (
    echo "No directory specified"
    sleep 1
    exit 1
)

cd "$DIR" || exit 1
tmux rename-window "$(dirname $DIR | sed "s|$HOME|~|" | sed -E 's/([^\/])[A-Za-z_]+/\1/g')/$(basename $DIR)"
exec $SHELL
