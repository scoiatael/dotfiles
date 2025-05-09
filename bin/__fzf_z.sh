#!/usr/bin/env zsh

# Interactive directory jump using zoxide and fzf
# Select a directory with fzf and cd into it

# Get directory list from zoxide
dir=$(zoxide query -l | \
    fzf-tmux --preview 'echo {}; exa -la {}' --layout=reverse --preview-window "top:3:wrap")

# Change to selected directory if one was chosen
if [[ -n "$dir" ]]; then
    cd "$dir"
fi

# Return control to shell
exec $SHELL
