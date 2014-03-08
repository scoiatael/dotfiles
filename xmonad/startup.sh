#!/bin/sh

myConfigDir=$HOME/.xmonad/confs/
myTerminal=/usr/bin/urxvtc
# Arch Linux lua-cairo fix
luaDir=/usr/lib/lua/5.1/

xscreensaver -no-splash &
sleep 1 && urxvtd &
sleep 1 && cd $luaDIR && conky -c "$myConfigDir"conkyStats.conf &
sleep 1 && $myTerminal &
syndaemon -k -d -t 1.0 &
setxkbmap pl
VBoxClient-all &
xmodmap "$myConfigDir"xmodmap &
