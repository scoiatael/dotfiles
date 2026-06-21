{
  "Print".screenshot = [ ];
  "Alt+Print".screenshot-window = [ ];

  # Niri
  "Mod+Shift+Slash".show-hotkey-overlay = [ ];
  "Ctrl+Alt+Delete".quit = [ ];
  "Mod+O".toggle-overview = [ ];
  "Mod+Q".close-window = [ ];
  "Mod+Alt+Q".close-window = [ ];

  # Applications
  "Mod+B".spawn = "zen-beta";
  "Mod+T".spawn = "wezterm";
  "Mod+S".spawn = [
    "wezterm"
    "start"
    "--class"
    "term-scratchpad"
  ];
  "Mod+E".spawn = [
    "dolphin"
    "--new-window"
  ];

  # Noctalia
  "Alt+Space".spawn-sh = "noctalia msg panel-toggle launcher";
  "Mod+A".spawn-sh = "noctalia msg panel-toggle control-center";
  "Mod+Alt+L".spawn-sh = "noctalia msg session lock";
  "Mod+Pause".spawn-sh = "noctalia msg notification-dnd-toggle";
  "Pause".spawn-sh = "noctalia msg mic-mute";
  "Ctrl+Print".spawn-sh = "noctalia msg screenshot-fullscreen pick";

  # Volume Up
  XF86AudioRaiseVolume = {
    _props.allow-when-locked = true;
    spawn = [
      "noctalia"
      "msg"
      "volume-up 5"
    ];
  };

  # Volume Down
  XF86AudioLowerVolume = {
    _props.allow-when-locked = true;
    spawn = [
      "noctalia"
      "msg"
      "volume-down 5"
    ];
  };

  # Volume Mute
  XF86AudioMute = {
    _props.allow-when-locked = true;
    spawn = [
      "noctalia"
      "msg"
      "volume-mute"
    ];
  };

  # Media Play/Pause (Toggle)
  XF86AudioPlay = {
    _props.allow-when-locked = true;
    spawn = [
      "noctalia"
      "msg"
      "media"
      "toggle"
    ];
  };

  # Media Play/Pause (Toggle)
  XF86AudioPause = {
    _props.allow-when-locked = true;
    spawn = [
      "noctalia"
      "msg"
      "media"
      "toggle"
    ];
  };

  # Media Stop
  XF86AudioStop = {
    _props.allow-when-locked = true;
    spawn = [
      "noctalia"
      "msg"
      "media"
      "stop"
    ];
  };

  # Media Previous
  XF86AudioPrev = {
    _props.allow-when-locked = true;
    spawn = [
      "noctalia"
      "msg"
      "media"
      "previous"
    ];
  };

  # Media Next
  XF86AudioNext = {
    _props.allow-when-locked = true;
    spawn = [
      "noctalia"
      "msg"
      "media"
      "next"
    ];
  };
  # Focus Columns
  "Mod+Left".focus-column-left = [ ];
  "Mod+Right".focus-column-right = [ ];
  "Mod+Down".focus-window-down = [ ];
  "Mod+Up".focus-window-up = [ ];
  "Mod+Shift+Left".focus-monitor-left = [ ];
  "Mod+Shift+Right".focus-monitor-right = [ ];
  "Mod+Home".focus-column-first = [ ];
  "Mod+End".focus-column-last = [ ];

  # Move Columns
  "Mod+C".center-column = [ ];
  "Mod+Ctrl+C".center-visible-columns = [ ];
  "Mod+Ctrl+Left".move-column-left = [ ];
  "Mod+Ctrl+Right".move-column-right = [ ];
  "Mod+Ctrl+Down".move-window-down = [ ];
  "Mod+Ctrl+Up".move-window-up = [ ];
  "Mod+Shift+Ctrl+Left".move-column-to-monitor-left = [ ];
  "Mod+Shift+Ctrl+Right".move-column-to-monitor-right = [ ];
  "Mod+Ctrl+Home".move-column-to-first = [ ];
  "Mod+Ctrl+End".move-column-to-last = [ ];

  # Resize Columns
  "Mod+F".maximize-column = [ ];
  "Mod+M".maximize-window-to-edges = [ ];
  "Mod+Shift+F".fullscreen-window = [ ];
  "Mod+Ctrl+Shift+F".toggle-windowed-fullscreen = [ ];
  "Mod+Minus".set-column-width = "-10%";
  "Mod+Equal".set-column-width = "+10%";
  "Mod+Shift+Minus".set-window-height = "-10%";
  "Mod+Shift+Equal".set-window-height = "+10%";
  "Mod+R".switch-preset-column-width = [ ];
  "Mod+Shift+R".switch-preset-column-width-back = [ ];
  "Mod+Ctrl+Shift+R".switch-preset-window-height = [ ];

  # Expel/Consume Columns
  "Mod+V".toggle-window-floating = [ ];
  "Mod+Shift+V".switch-focus-between-floating-and-tiling = [ ];
  "Mod+W".toggle-column-tabbed-display = [ ];
  "Mod+BracketLeft".consume-or-expel-window-left = [ ];
  "Mod+BracketRight".consume-or-expel-window-right = [ ];
  "Mod+Comma".consume-window-into-column = [ ];
  "Mod+Period".expel-window-from-column = [ ];

  # HJKLUI Binds
  "Mod+H".focus-column-left = [ ];
  "Mod+L".focus-column-right = [ ];
  "Mod+K".focus-window-down = [ ];
  "Mod+J".focus-window-up = [ ];
  "Mod+Ctrl+H".move-column-left = [ ];
  "Mod+Ctrl+L".move-column-right = [ ];
  "Mod+Ctrl+K".move-window-down = [ ];
  "Mod+Ctrl+J".move-window-up = [ ];
  "Mod+U".focus-workspace-down = [ ];
  "Mod+I".focus-workspace-up = [ ];
  "Mod+Shift+U".move-workspace-down = [ ];
  "Mod+Shift+I".move-workspace-up = [ ];
  "Mod+Ctrl+U".move-column-to-workspace-down = [ ];
  "Mod+Ctrl+I".move-column-to-workspace-up = [ ];
  # Workspace Navigation & Column Movements
  "Mod+Page_Down".focus-workspace-down = [ ];
  "Mod+Page_Up".focus-workspace-up = [ ];
  "Mod+Shift+Page_Down".move-workspace-down = [ ];
  "Mod+Shift+Page_Up".move-workspace-up = [ ];
  "Mod+Ctrl+Page_Down".move-column-to-workspace-down = [ ];
  "Mod+Ctrl+Page_Up".move-column-to-workspace-up = [ ];

  # Workspace Number Navigation & Column Movements
  "Mod+1".focus-workspace = 1;
  "Mod+2".focus-workspace = 2;
  "Mod+3".focus-workspace = 3;
  "Mod+4".focus-workspace = 4;
  "Mod+5".focus-workspace = 5;
  "Mod+6".focus-workspace = 6;
  "Mod+7".focus-workspace = 7;
  "Mod+8".focus-workspace = 8;
  "Mod+9".focus-workspace = 9;
  "Mod+Ctrl+1".move-column-to-workspace = 1;
  "Mod+Ctrl+2".move-column-to-workspace = 2;
  "Mod+Ctrl+3".move-column-to-workspace = 3;
  "Mod+Ctrl+4".move-column-to-workspace = 4;
  "Mod+Ctrl+5".move-column-to-workspace = 5;
  "Mod+Ctrl+6".move-column-to-workspace = 6;
  "Mod+Ctrl+7".move-column-to-workspace = 7;
  "Mod+Ctrl+8".move-column-to-workspace = 8;
  "Mod+Ctrl+9".move-column-to-workspace = 9;

  # Column Mouse Scroll Right
  "Mod+WheelScrollDown" = {
    _props.cooldown-ms = 150;
    focus-column-right = [ ];
  };

  # Column Mouse Scroll Left
  "Mod+WheelScrollUp" = {
    _props.cooldown-ms = 150;
    focus-column-left = [ ];
  };

  # Workspace Mouse Scroll Down
  "Mod+Ctrl+WheelScrollDown" = {
    _props.cooldown-ms = 150;
    focus-workspace-down = [ ];
  };

  # Workspace Mouse Scroll Up
  "Mod+Ctrl+WheelScrollUp" = {
    _props.cooldown-ms = 150;
    focus-workspace-up = [ ];
  };
}
