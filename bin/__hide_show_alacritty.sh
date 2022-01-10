#!/usr/bin/env elvish

var ALACRITTY_FOCUSED = (yabai -m query --windows | jq '.[] | select(.app=="Alacritty") | .focused // ."has-focus"')
var ALACRITTY_ID = (yabai -m query --windows | jq '.[] | select(.app=="Alacritty") | .id')
if (or (==s true $ALACRITTY_FOCUSED) (== 1 $ALACRITTY_FOCUSED)) {
  yabai -m window --minimize $ALACRITTY_ID
  exec yabai -m window --focus last
} else {
  exec yabai -m window --focus $ALACRITTY_ID
}
