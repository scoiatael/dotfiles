#!/usr/bin/env bash

HOST=$(
    awk '{ split($1, A, ","); print(A[1]) }' <"$HOME/.ssh/known_hosts" |
        fzf-tmux-popup \
            --preview 'host {}' \
            --layout=reverse \
            --preview-window "top:3:wrap"
)

if test -z "$HOST"; then
    echo "No host specified"
    sleep 1
    exit 1
fi

function join_by { local IFS="$1"; shift; echo "$*"; }

IFS=. read -ra ARR <<<"$HOST"
BASE="$ARR"
DOMAIN=("${ARR[@]:1}")
DOMAIN=$(join_by $'\n' "${DOMAIN[@]}" | tac | cut -c 1| tr "\n" '.')

tmux rename-window "$DOMAIN$BASE"
ssh "$HOST" || sleep 60
