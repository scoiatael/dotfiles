#!/usr/bin/env elvish

var ALACRITTY_FOCUSED = (yabai -m query --windows | jq '.[] | select(.app=="Alacritty") | .focused')
var ALACRITTY_ID = (yabai -m query --windows | jq '.[] | select(.app=="Alacritty") | .id')
if ?(test 1 -eq $ALACRITTY_FOCUSED) {
  yabai -m window --minimize $ALACRITTY_ID
  exec yabai -m window --focus last
} else {
  exec yabai -m window --focus $ALACRITTY_ID
}
