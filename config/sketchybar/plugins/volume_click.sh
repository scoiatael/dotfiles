#!/usr/bin/env bash

NAME="${NAME:-volume}"
SwitchAudioSource="/opt/homebrew/bin/SwitchAudioSource"
toggle_devices() {
  source "$HOME/dotfiles/config/sketchybar/colors.sh"

  args=(--remove '/volume.device\.*/' --set "$NAME" popup.drawing=toggle)
  COUNTER=0
  CURRENT="$($SwitchAudioSource -t output -c)"
  while IFS= read -r device; do
    COLOR=$GREY
    if [ "${device}" = "$CURRENT" ]; then
      COLOR=$WHITE
    fi
    args+=(--add item volume.device.$COUNTER popup."$NAME" \
           --set volume.device.$COUNTER label="${device}" \
                                        label.color="$COLOR" \
                 click_script="$SwitchAudioSource -s \"${device}\" && sketchybar --set /volume.device\.*/ label.color=$GREY --set \$NAME label.color=$WHITE --set $NAME popup.drawing=off")
    COUNTER=$((COUNTER+1))
  done <<< "$($SwitchAudioSource -a -t output)"

  sketchybar -m "${args[@]}" > /dev/null
}

toggle_devices
