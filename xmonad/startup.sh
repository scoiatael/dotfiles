#!/bin/sh

myConfigDir=$HOME/.xmonad/confs/
myTerminal=/usr/bin/urxvtc

xscreensaver -no-splash &
sleep 1 && urxvtd &
sleep 1 && conky -c "$myConfigDir"conkyMessages.conf &
sleep 1 && conky -c "$myConfigDir"conkyStats.conf &
sleep 1 && $myTerminal &
sleep 1 && syndaemon -k -d -t 1.0 &
sleep 1 && xmodmap "$myConfigDir"xmodmap &
setxkbmap pl &
