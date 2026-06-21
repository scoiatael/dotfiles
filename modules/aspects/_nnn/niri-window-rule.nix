[
  {
    #/ Rounded corners for a modern look.
    geometry-corner-radius = 20;

    #/ Clips window contents to the rounded corner boundaries.
    clip-to-geometry = true;
  }

  #/ Floating Noctalia settings window.
  {
    match._props.app-id = "dev.noctalia.Noctalia.Settings";
    open-floating = true;
    default-column-width = {
      fixed = 1080;
    };
    default-window-height = {
      fixed = 920;
    };
  }
  {
    match._props.app-id = "term-scratchpad";
    open-floating = true;
    default-column-width.proportion = 0.6;
    default-window-height.proportion = 0.6;

    default-floating-position._props = {
      x = 410;
      y = 222;
      relative-to = "top-left";
    };
  }

  # Steam Notifications Bottom Right Corner - Main Monitor
  {
    match._props = {
      app-id = "steam";
      title = "^notificationtoasts_\\d+_desktop$";
    };

    open-floating = true;
    open-focused = false;

    default-floating-position._props = {
      x = 20;
      y = 20;
      relative-to = "bottom-right";
    };
  }

  # Firefox PIP
  {
    match._props = {
      title = "^Picture-in-Picture$";
    };

    open-floating = true;
    open-focused = false;

    default-floating-position._props = {
      x = 10;
      y = 10;
      relative-to = "bottom-right";
    };
  }
]
