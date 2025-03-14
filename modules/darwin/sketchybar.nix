{ config, lib, pkgs, ... }:

let
  sketchybar-helper = (pkgs.callPackage ../../packages/sketchybar-helper { });
  colors = ../../config/sketchybar/colors.sh;
  icons = ../../config/sketchybar/icons.sh;
  plugin_dir = ../../config/sketchybar/plugins;
  item_dir = ../../config/sketchybar/items;
in {
  # Helper for CPU
  launchd.user.agents.sketchybar-helper = {
    path = [ sketchybar-helper config.environment.systemPath ];
    serviceConfig.ProgramArguments = [
      "/bin/sh"
      "-c"
      "source ${colors} && ${sketchybar-helper}/bin/sketchybar-helper git.scoiatael.helper"
    ];
    serviceConfig.KeepAlive = true;
    serviceConfig.RunAtLoad = true;
  };

  fonts.packages = [ pkgs.nerd-fonts.jetbrains-mono ];

  services.sketchybar = {
    enable = true;
    config = let
      before = ''
        # source: https://github.com/FelixKratz/dotfiles

        source "${colors}" # Loads all defined colors
        source "${icons}" # Loads all defined icons

        PLUGIN_DIR="${plugin_dir}"
        ITEM_DIR="${item_dir}"
        FONT="JetBrainsMono Nerd Font:Regular"

        # Setting up and starting the helper process
        HELPER=git.scoiatael.helper

        ##### Bar Appearance #####
        # Configuring the general appearance of the bar, these are only some of the
        # options available. For all options see:
        # https://felixkratz.github.io/SketchyBar/config/bar
        # If you are looking for other colors, see the color picker:
        # https://felixkratz.github.io/SketchyBar/config/tricks#color-picker

        sketchybar --bar height=36        \
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
        defaults=(
          icon.font="$FONT:12.0"
          icon.color=$ICON_COLOR
          label.font="$FONT:12.0"
          label.color=$LABEL_COLOR
          padding_left=5
          padding_right=5
          label.padding_left=4
          label.padding_right=4
          icon.padding_left=4
          icon.padding_right=4
          popup.background.border_width=2
          popup.background.corner_radius=9
          popup.background.border_color=$POPUP_BORDER_COLOR
          popup.background.color=$POPUP_BACKGROUND_COLOR
          popup.blur_radius=20
          popup.background.shadow.drawing=on
        )

        sketchybar --default "''${defaults[@]}"
        source "$ITEM_DIR/apple.sh"
      '';
      after = ''
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
                                  icon=$CLOCK                           \
                                  script="$PLUGIN_DIR/clock.sh"         \
                                                                        \
                    --add item wifi right                               \
                    --set wifi    script="$PLUGIN_DIR/wifi.sh"          \
                                  icon=$WIFI                               \
                    --subscribe wifi wifi_change                        \
                                                                        \
                    --add item volume right                             \
                    --set volume  script="$PLUGIN_DIR/volume.sh"        \
                                  click_script="$PLUGIN_DIR/volume_click.sh" \
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
    in lib.mkMerge [ (lib.mkBefore before) (lib.mkAfter after) ];
  };
}
