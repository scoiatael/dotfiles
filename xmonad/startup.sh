#!/bin/sh

myConfigDir=$HOME/.xmonad/confs/
myTerminal=/usr/bin/urxvtc

xscreensaver -no-splash &
urxvtd &
conky -c "$myConfigDir"conkyMessages.conf &
conky -c "$myConfigDir"conkyStats.conf &
sleep 1 && $myTerminal &
syndaemon -k -d -t 1.0
