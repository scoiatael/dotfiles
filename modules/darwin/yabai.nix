{ config, lib, pkgs, ... }:

with lib; {
  options.hardware.notch = lib.mkOption {
    type = with lib.types; bool;
    default = false;
  };

  config = {
    services.yabai = {
      enable = true;
      package = pkgs.yabai;
      enableScriptingAddition = false;
      config = {
        focus_follows_mouse = "autoraise";
        mouse_follows_focus = "on";
        window_placement = "second_child";
        window_opacity = "off";
        window_opacity_duration = "0.0";
        window_border = "on";
        window_border_placement = "inset";
        window_border_width = 2;
        window_border_radius = 3;
        active_window_border_topmost = "off";
        window_topmost = "on";
        window_shadow = "float";
        active_window_border_color = "0xff5c7e81";
        normal_window_border_color = "0xff505050";
        insert_window_border_color = "0xffd75f5f";
        active_window_opacity = "1.0";
        normal_window_opacity = "1.0";
        split_ratio = "0.50";
        auto_balance = "on";
        mouse_action1 = "move";
        mouse_action2 = "resize";
        layout = "bsp";
        top_padding = if config.hardware.notch then 0 else 36;
        bottom_padding = 10;
        left_padding = 10;
        right_padding = 10;
        window_gap = 5;
        mouse_modifier = "ctrl";
        mouse_drop_action = "stack";
      };

      extraConfig = ''
        # global settings
        yabai -m config insert_feedback_color 0xffd75f5f

        # General app settings
        yabai -m rule --add app="^Bitwarden$" manage=off grid=128:128:16:16:96:96

        # https://github.com/koekeishiya/yabai/issues/1317
        # yabai -m signal --add event=window_created action='yabai -m query --windows --window $YABAI_WINDOW_ID | jq -er ".\"can-resize\" or .\"is-floating\"" || yabai -m window $YABAI_WINDOW_ID --toggle float'
        yabai -m signal --add event=window_minimized active=yes action="if \$(yabai -m query --windows --window \$YABAI_WINDOW_ID | jq -r '.\"is-floating\"'); then yabai -m query --windows --window &> /dev/null || yabai -m window --focus mouse &> /dev/null || yabai -m window --focus \$(yabai -m query --windows --space | jq .[0].id) &> /dev/null; fi"

        echo "yabai configuration loaded.."
      '';
    };
    # services.skhd = {
    #   enable = true;
    #   package = pkgs.skhd;
    #   # TODO: this has to be linked to ~/.skhdrc, otherwise will be ignored
    #   skhdConfig = ''
    #     # focus window
    #     cmd - h : yabai -m window --focus west
    #     cmd - j : yabai -m window --focus south
    #     cmd - k : yabai -m window --focus north
    #     cmd - l : yabai -m window --focus east

    #     # colemak-dh
    #     # cmd - m : yabai -m window --focus west
    #     # cmd - n : yabai -m window --focus south
    #     # cmd - e : yabai -m window --focus north
    #     # cmd - i : yabai -m window --focus east

    #     # swap managed window
    #     ctrl + cmd - h : yabai -m window --swap west
    #     ctrl + cmd - j : yabai -m window --swap south
    #     ctrl + cmd - k : yabai -m window --swap north
    #     ctrl + cmd - l : yabai -m window --swap east

    #     # move managed window
    #     shift + cmd - h : yabai -m window --warp west
    #     shift + cmd - j : yabai -m window --warp south
    #     shift + cmd - k : yabai -m window --warp north
    #     shift + cmd - l : yabai -m window --warp east

    #     # balance size of windows
    #     cmd + ctrl - 0 : yabai -m space --balance

    #     # focus monitor
    #     cmd  - 0x21  : yabai -m display --focus prev
    #     cmd  - 0x1E  : yabai -m display --focus next
    #     # ctrl + alt - 3  : yabai -m display --focus 3

    #     # send window to monitor and follow focus
    #     ctrl + cmd - 0x21  : yabai -m window --display prev; yabai -m display --focus prev
    #     ctrl + cmd - 0x1E  : yabai -m window --display next; yabai -m display --focus next
    #     # ctrl + cmd - 1  : yabai -m window --display 1; yabai -m display --focus 1

    #     # move floating window
    #     # shift + ctrl - a : yabai -m window --move rel:-20:0
    #     # shift + ctrl - s : yabai -m window --move rel:0:20

    #     # increase window size
    #     # shift + alt - a : yabai -m window --resize left:-20:0
    #     # shift + alt - w : yabai -m window --resize top:0:-20

    #     # decrease window size
    #     # shift + cmd - s : yabai -m window --resize bottom:0:-20
    #     # shift + cmd - w : yabai -m window --resize top:0:20

    #     # set insertion point in focused container
    #     # ctrl + alt - h : yabai -m window --insert west

    #     # toggle window zoom
    #     cmd + shift - return : yabai -m window --toggle zoom-parent
    #     cmd + ctrl - return : yabai -m window --toggle zoom-fullscreen

    #     # toggle window split type
    #     cmd - 0x2C  : yabai -m window --toggle split

    #     # float / unfloat window and center on screen
    #     alt - t : yabai -m window --toggle float;\
    #               yabai -m window --grid 4:4:1:1:2:2

    #     # toggle sticky(+float), topmost, picture-in-picture
    #     alt - p : yabai -m window --toggle sticky;\
    #               yabai -m window --toggle topmost;\
    #               yabai -m window --toggle pip

    #     # https://github.com/koekeishiya/yabai/wiki/Tips-and-tricks#quickly-restart-the-yabai-launch-agent
    #     cmd + shift - c : launchctl kickstart -k "gui/''${UID}/homebrew.mxcl.yabai"

    #     # minimize window
    #     cmd - m : yabai -m window --minimize; yabai -m window --focus mouse
    #   '';
    # };
  };
}
