#!/usr/bin/env elvish -norc

use github.com/zzamboni/elvish-modules/util

var ALACRITTY = (
  yabai -m query --windows |
    from-json |
    util:select { |x| ==s $x['app'] Alacritty } (all)
)
var ALACRITTY_ID = (exact-num $ALACRITTY["id"])
if (or (eq $true (util:path-in $ALACRITTY ["has-focus"] &default=false)) (== 1 (exact-num (util:path-in $ALACRITTY ["focused"] &default=0)))) {
  yabai -m window --minimize $ALACRITTY_ID
  exec yabai -m window --focus last
} else {
  exec yabai -m window --focus $ALACRITTY_ID
}
