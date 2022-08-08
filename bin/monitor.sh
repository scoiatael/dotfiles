#!/usr/bin/env elvish
# inspired by https://github.com/ahghanbari/External_monitor_brightness/blob/main/screen.sh

if (not (has-external ddcutil)) {
    echo "install ddcutil"
    exit 127
}

if (not (has-external notify-send)) {
    echo "install libnotify"
    exit 127
}

var raw = (ddcutil getvcp 10 -t --bus 14 | awk '{ print $4 }' | one)
var br = (exact-num $raw)
fn setvcp {
    ddcutil setvcp 10 $br
    notify-send -i monitor -t 300 "Brightness: " $br
}

if (> (count $args) 0) {
    if (==s $args[0] up ) {
        set br = (+ $br 10)
        setvcp
    }

    if (==s $args[0] down ) {
        set br = (- $br 10)
        setvcp
    }
}

put [ &percentage=$br ] | to-json | jq --unbuffered --compact-output '.'
