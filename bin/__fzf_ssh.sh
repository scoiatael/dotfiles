#!/usr/bin/env zsh

HOST=$(cat ~/.ssh/known_hosts | 
       awk '{print $1}' | 
       sed 's/,.*//' | 
       grep -v '^[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}\.[0-9]\{1,3\}$' | 
       grep '\.' | 
       sort | 
       uniq | 
       fzf-tmux --preview-window "top:3:wrap" --preview 'host {}' --layout=reverse)

if [[ -n "$HOST" ]]; then
  BASE=$(echo "$HOST" | cut -d '.' -f 1)
  DOMAIN=$(echo "$HOST" | cut -d '.' -f 2- | tac 2>/dev/null || rev | tr '.' '\n' | tr '\n' '.' | sed 's/\.$//g')
  
  tmux rename-window "$DOMAIN.$BASE"
  ssh "$HOST" || sleep 60
fi
