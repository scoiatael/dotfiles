#!/bin/sh

myConfigDir=$HOME/.xmonad/confs/
myTerminal=/usr/bin/urxvtc

xscreensaver -no-splash &
sleep 1 && urxvtd &
sleep 1 && conky -c "$myConfigDir"conkyMessages.conf &
sleep 1 && conky -c "$myConfigDir"conkyStats.conf &
sleep 1 && $myTerminal &
syndaemon -k -d -t 1.0 &
setxkbmap pl
VBoxClient-all &
xmodmap "$myConfigDir"xmodmap &
