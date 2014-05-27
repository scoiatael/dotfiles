#!/bin/sh

myConfigDir=$HOME/.xmonad/confs/
myTerminal=/usr/bin/urxvtc

xscreensaver -no-splash &
sleep 1 && (pgrep urxvtd || urxvtd) &
sleep 1 && conky -c "$myConfigDir"conkyStats.conf &
sleep 2 && $myTerminal &
syndaemon -k -d -t 1.0 &
xmodmap "$myConfigDir"xmodmap &
setxkbmap pl &
xmodmap "$myConfigDir"xmodmap &
