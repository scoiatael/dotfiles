#!/bin/sh

myConfigDir=$HOME/.xmonad/confs/
myTerminal=/usr/bin/urxvtc

( echo "Merging $HOME/.Xdefaults" && xrdb $HOME/.Xdefaults ) | logger &
sleep 1 && (pgrep urxvtd || urxvtd) &
#sleep 1 && conky -c "$myConfigDir"conkyStats.conf &
sleep 2 && (pgrep urxvtc || urxvtc) &
syndaemon -k -d -t 1.0 &
setxkbmap pl -option "caps:swapescape" &
