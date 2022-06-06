#!/usr/bin/env elvish
# inspired by https://github.com/ahghanbari/External_monitor_brightness/blob/main/screen.sh

if (not (has-external xrandr)) {
    echo "install xrandr"
    exit 127
}

if (not (has-external notify-send)) {
    echo "install libnotify"
    exit 127
}

var br = (xrandr --prop --verbose | grep -A10 " connected primary" | grep "Brightness" | awk '{ print $2 }')
var dev = (xrandr --prop --verbose | grep " connected primary" | awk '{ print $1 }')

if (==s $args[0] up ) {
    set br = (+ $br 0.1)
}

if (==s $args[0] down ) {
    set br = (- $br 0.1)
}

xrandr --output $dev --brightness $br
notify-send -i monitor -t 300 "Brightness: " $br
