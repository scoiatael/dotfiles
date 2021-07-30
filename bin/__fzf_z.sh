#!/usr/bin/env bash

DIR=$(
    zoxide query -l |
        fzf-tmux-popup \
            --preview 'echo {}; exa {}' \
            --layout=reverse \
            --preview-window "top:3:wrap"
)

if test -z "$DIR"; then
    echo "No directory specified"
    sleep 1
    exit 1
fi

cd "$DIR" || exit 1
tmux rename-window "$(dirname $DIR | sed "s|$HOME|~|" | sed -E 's/([^\/])[A-Za-z_]+/\1/g')/$(basename $DIR)"
exec $SHELL
