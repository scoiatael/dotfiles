#!/usr/bin/env sh

if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
    sketchybar --set space.$1 background.drawing=on
else
    sketchybar --set space.$1 background.drawing=off
fi

COUNT="$(/run/current-system/sw/bin/aerospace list-windows --workspace "$1" --count)"
if test -n "$COUNT"; then
    if test "$COUNT" -eq 0; then
        sketchybar --set space.$1 label.color=0xff181926
    else
        sketchybar --set space.$1 label.color=0xffcad3f5
    fi
fi
