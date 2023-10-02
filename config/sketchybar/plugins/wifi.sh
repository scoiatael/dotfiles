#!/bin/sh

# The wifi_change event supplies a $INFO variable in which the current SSID
# is passed to the script.
update() {
  source "$CONFIG_DIR/icons.sh"
  INFO="$(/System/Library/PrivateFrameworks/Apple80211.framework/Resources/airport -I | awk -F ' SSID: '  '/ SSID: / {print $2}')"
  LABEL="$INFO ($(ipconfig getifaddr en0))"
  ICON="$([ -n "$INFO" ] && echo "$WIFI_CONNECTED" || echo "$WIFI_DISCONNECTED")"

  sketchybar --set $NAME icon="$ICON" label="$LABEL"
}


update
