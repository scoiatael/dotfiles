#!/bin/sh

myConfigDir=$HOME/.xmonad/confs/
myTerminal=/usr/bin/urxvtc

( echo "Merging $HOME/.Xdefaults" && xrdb $HOME/.Xdefaults ) | logger &
sleep 1 && (pgrep urxvtd || urxvtd) &
sleep 1 && conky -c "$myConfigDir"conkyStats.conf &
<<<<<<< HEAD
sleep 2 && urxvtc &
=======
sleep 2 && (pgrep urxvtc || urxvtc) &
>>>>>>> 49dd7d5e7e4395956034eff74f3e51ae637fe2f5
syndaemon -k -d -t 1.0 &
setxkbmap pl -option "caps:swapescape" &
