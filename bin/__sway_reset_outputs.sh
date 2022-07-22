#!/usr/bin/env bash
LAPTOP='eDP-1'
if grep -q open /proc/acpi/button/lid/LID0/state; then
    swaymsg output "$LAPTOP" enable
else
    swaymsg output "$LAPTOP" disable
fi
