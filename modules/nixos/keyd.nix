{ config, lib, pkgs, ... }:

{
  # https://wiki.nixos.org/wiki/Keyd
  services.keyd = { enable = true; };

  # Optional, but makes sure that when you type the make palm rejection work with keyd
  # https://github.com/rvaiya/keyd/issues/723
  environment.etc."libinput/local-overrides.quirks".text = ''
    [Serial Keyboards]
    MatchUdevType=keyboard
    MatchName=keyd virtual keyboard
    AttrKeyboardIntegration=internal
  '';

  environment.etc."keyd/default.conf".text = ''
    [ids]

    *

    [main]

    # Maps capslock to escape when pressed and layer "capslock" when held.
    capslock = overload(capslock, esc)

    # Remaps the escape key to capslock
    esc = capslock

    # Remap simple modifiers to oneshot
    shift = oneshot(shift)
    control = oneshot(control)

    leftalt = oneshot(alt)
    rightalt = oneshot(altgr)

    # Make meta behave more like macOS
    meta = oneshot(meta)


    # https://github.com/rvaiya/keyd/blob/6dc2d5c4ea76802fd192b143bdd53b1787fd6deb/docs/keyd.scdoc#L128
    # Layer named "capslock" - defaults to control+KEY if not found
    [capslock:C]

    h = left
    j = down
    k = up
    l = right

    [meta:M]

    c = S-C-c
    v = S-C-v
    t = S-C-t
  '';
}
