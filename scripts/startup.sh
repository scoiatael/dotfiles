#!/bin/sh

Xdefaults="$HOME/.Xdefaults"

function lazy_start {
  sleep $1 && (pgrep $2 || $2) &
}

( echo "Merging $Xdefaults" && xrdb $Xdefaults ) | logger &
#lazy_start 1 urxvtd
#lazy_start 2 urxvtc
#emacs --daemon &
#syndaemon -k -d -i 1.0 &
#setxkbmap pl -option "caps:swapescape" &
#unclutter &
