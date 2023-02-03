{ pkgs, ... }:
let top_padding = 36;
in {
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    vim
    pinentry_mac
    (callPackage ../packages/sketchybar-helper { })
  ];

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
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      layout = "bsp";
      top_padding = top_padding;
      bottom_padding = 2;
      left_padding = 2;
      right_padding = 2;
      window_gap = 5;
    };

    extraConfig = ''
      # rules
      yabai -m rule --add app='System Preferences' manage=off

      # Any other arbitrary config here
    '';
  };

  services.sketchybar = {
    enable = true;
    package = pkgs.sketchybar.overrideAttrs (final: old: rec {
      version = "2.14.0";

      src = pkgs.fetchFromGitHub {
        owner = "FelixKratz";
        repo = "SketchyBar";
        rev = "v${version}";
        sha256 = "sha256-2uViQ3/gQcEgUbBK87qDcB8lcn1xFHWCaTDJPb4y3Ws=";
      };
    });
    config = ''
      # source: https://github.com/FelixKratz/dotfiles

      source "$HOME/dotfiles/config/sketchybar/colors.sh" # Loads all defined colors
      source "$HOME/dotfiles/config/sketchybar/icons.sh" # Loads all defined icons

      PLUGIN_DIR="$HOME/dotfiles/config/sketchybar/plugins"
      ITEM_DIR="$HOME/dotfiles/config/sketchybar/items"
      FONT="JetBrainsMono Nerd Font:Regular"

      # Setting up and starting the helper process
      HELPER=git.scoiatael.helper
      killall sketchybar-helper
      sketchybar-helper $HELPER > /dev/null 2>&1 &

      ##### Bar Appearance #####
      # Configuring the general appearance of the bar, these are only some of the
      # options available. For all options see:
      # https://felixkratz.github.io/SketchyBar/config/bar
      # If you are looking for other colors, see the color picker:
      # https://felixkratz.github.io/SketchyBar/config/tricks#color-picker

      sketchybar --bar height=${toString top_padding}        \
                       blur_radius=30   \
                       position=top     \
                       sticky=off       \
                       padding_left=10  \
                       padding_right=10 \
                       color=$BAR_COLOR

      ##### Changing Defaults #####
      # We now change some default values that are applied to all further items
      # For a full list of all available item properties see:
      # https://felixkratz.github.io/SketchyBar/config/items

      sketchybar --default icon.font="$FONT:12.0"  \
                           icon.color=$ICON_COLOR                 \
                           label.font="$FONT:12.0" \
                           label.color=$LABEL_COLOR                \
                           padding_left=5                        \
                           padding_right=5                       \
                           label.padding_left=4                  \
                           label.padding_right=4                 \
                           icon.padding_left=4                   \
                           icon.padding_right=4

      ##### Adding Mission Control Space Indicators #####
      # Now we add some mission control spaces:
      # https://felixkratz.github.io/SketchyBar/config/components#space----associate-mission-control-spaces-with-an-item
      # to indicate active and available mission control spaces
    '' + (builtins.concatStringsSep "\n" (builtins.attrValues (builtins.mapAttrs
      (sid: icon: ''
        sketchybar --add space space.${sid} left                               \
                   --set space.${sid} associated_space=${sid}                  \
                                    icon=${icon}                               \
                                    background.color=0xBACKGROUND_1                \
                                    background.corner_radius=5                 \
                                    background.height=20                       \
                                    background.drawing=off                     \
                                    label.drawing=off                          \
                                    script="$PLUGIN_DIR/space.sh"              \
                                    click_script="yabai -m space --focus ${sid}"
      '') {
        "1" = "1";
        "2" = "2";
        "3" = "3";
        "4" = "4";
        "5" = "5";
        "6" = "6";
        "7" = "7";
        "8" = "8";
        "9" = "9";
        "10" = "10";
      }))) + ''
        ##### Adding Left Items #####
        # We add some regular items to the left side of the bar
        # only the properties deviating from the current defaults need to be set

        sketchybar --add item space_separator left                         \
                   --set space_separator icon=                            \
                                         padding_left=10                   \
                                         padding_right=10                  \
                                         label.drawing=off                 \
                                                                           \
                   --add item front_app left                               \
                   --set front_app       script="$PLUGIN_DIR/front_app.sh" \
                                         icon.drawing=off                  \
                   --subscribe front_app front_app_switched

        ##### Adding Right Items #####
        # In the same way as the left items we can add items to the right side.
        # Additional position (e.g. center) are available, see:
        # https://felixkratz.github.io/SketchyBar/config/items#adding-items-to-sketchybar

        # Some items refresh on a fixed cycle, e.g. the clock runs its script once
        # every 10s. Other items respond to events they subscribe to, e.g. the
        # volume.sh script is only executed once an actual change in system audio
        # volume is registered. More info about the event system can be found here:
        # https://felixkratz.github.io/SketchyBar/config/events

        sketchybar --add item clock right                              \
                   --set clock   update_freq=10                        \
                                 icon=                                \
                                 script="$PLUGIN_DIR/clock.sh"         \
                                                                       \
                   --add item wifi right                               \
                   --set wifi    script="$PLUGIN_DIR/wifi.sh"          \
                                 icon=$WIFI                               \
                   --subscribe wifi wifi_change                        \
                                                                       \
                   --add item volume right                             \
                   --set volume  script="$PLUGIN_DIR/volume.sh"        \
                   --subscribe volume volume_change                    \
                                                                       \
                   --add item battery right                            \
                   --set battery script="$PLUGIN_DIR/battery.sh"       \
                                 update_freq=120                       \
                   --subscribe battery system_woke power_source_change

        source "$ITEM_DIR/cpu.sh"

        ##### Finalizing Setup #####
        # The below command is only needed at the end of the initial configuration to
        # force all scripts to run the first time, it should never be run in an item script.

        sketchybar --update
      '';
  };

  # programs.fish.enable = true;
  programs.zsh.enable = true; # default shell on catalina
  programs.gnupg = {
    agent = {
      enable = true;
      enableSSHSupport = true;
    };
  };

  homebrew = {
    enable = true;
    casks = [
      "battle-net"
      "bitwarden"
      "iterm2"
      "karabiner-elements"
      "syncthing"
      "eloston-chromium"
    ];
  };

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  nix.package = pkgs.nix;
}
