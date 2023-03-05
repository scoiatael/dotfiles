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

      # global settings
      yabai -m config insert_feedback_color 0xffd75f5f

      # General app settings
      yabai -m rule --add app="^Bitwarden$" manage=off grid=128:128:16:16:96:96

      echo "yabai configuration loaded.."
    '';
  };
  services.skhd = {
    enable = true;
    package = pkgs.skhd;
    skhdConfig = ''
      # ################################################################ #
      # THE FOLLOWING IS AN EXPLANATION OF THE GRAMMAR THAT SKHD PARSES. #
      # FOR SIMPLE EXAMPLE MAPPINGS LOOK FURTHER DOWN THIS FILE..        #
      # ################################################################ #
      # https://github.com/koekeishiya/yabai/wiki/Commands

      # A list of all built-in modifier and literal keywords can
      # be found at https://github.com/koekeishiya/skhd/issues/1
      #
      # A hotkey is written according to the following rules:
      #
      #   hotkey       = <mode> '<' <action> | <action>
      #
      #   mode         = 'name of mode' | <mode> ',' <mode>
      #
      #   action       = <keysym> '[' <proc_map_lst> ']' | <keysym> '->' '[' <proc_map_lst> ']'
      #                  <keysym> ':' <command>          | <keysym> '->' ':' <command>
      #                  <keysym> ';' <mode>             | <keysym> '->' ';' <mode>
      #
      #   keysym       = <mod> '-' <key> | <key>
      #
      #   mod          = 'modifier keyword' | <mod> '+' <mod>
      #
      #   key          = <literal> | <keycode>
      #
      #   literal      = 'single letter or built-in keyword'
      #
      #   keycode      = 'apple keyboard kVK_<Key> values (0x3C)'
      #
      #   proc_map_lst = * <proc_map>
      #
      #   proc_map     = <string> ':' <command> | <string>     '~' |
      #                  '*'      ':' <command> | '*'          '~'
      #
      #   string       = '"' 'sequence of characters' '"'
      #
      #   command      = command is executed through '$SHELL -c' and
      #                  follows valid shell syntax. if the $SHELL environment
      #                  variable is not set, it will default to '/bin/bash'.
      #                  when bash is used, the ';' delimeter can be specified
      #                  to chain commands.
      #
      #                  to allow a command to extend into multiple lines,
      #                  prepend '\' at the end of the previous line.
      #
      #                  an EOL character signifies the end of the bind.
      #
      #   ->           = keypress is not consumed by skhd
      #
      #   *            = matches every application not specified in <proc_map_lst>
      #
      #   ~            = application is unbound and keypress is forwarded per usual, when specified in a <proc_map>
      #
      # A mode is declared according to the following rules:
      #
      #   mode_decl = '::' <name> '@' ':' <command> | '::' <name> ':' <command> |
      #               '::' <name> '@'               | '::' <name>
      #
      #   name      = desired name for this mode,
      #
      #   @         = capture keypresses regardless of being bound to an action
      #
      #   command   = command is executed through '$SHELL -c' and
      #               follows valid shell syntax. if the $SHELL environment
      #               variable is not set, it will default to '/bin/bash'.
      #               when bash is used, the ';' delimeter can be specified
      #               to chain commands.
      #
      #               to allow a command to extend into multiple lines,
      #               prepend '\' at the end of the previous line.
      #
      #               an EOL character signifies the end of the bind.

      # ############################################################### #
      # THE FOLLOWING SECTION CONTAIN SIMPLE MAPPINGS DEMONSTRATING HOW #
      # TO INTERACT WITH THE YABAI WM. THESE ARE SUPPOSED TO BE USED AS #
      # A REFERENCE ONLY, WHEN MAKING YOUR OWN CONFIGURATION..          #
      # ############################################################### #

      # focus window
      cmd - h : yabai -m window --focus west
      cmd - j : yabai -m window --focus south
      cmd - k : yabai -m window --focus north
      cmd - l : yabai -m window --focus east

      # colemak-dh
      cmd - m : yabai -m window --focus west
      cmd - n : yabai -m window --focus south
      cmd - e : yabai -m window --focus north
      cmd - i : yabai -m window --focus east

      # swap managed window
      ctrl + cmd - h : yabai -m window --swap west
      ctrl + cmd - j : yabai -m window --swap south
      ctrl + cmd - k : yabai -m window --swap north
      ctrl + cmd - l : yabai -m window --swap east

      # move managed window
      shift + cmd - h : yabai -m window --warp west
      shift + cmd - j : yabai -m window --warp south
      shift + cmd - k : yabai -m window --warp north
      shift + cmd - l : yabai -m window --warp east

      # balance size of windows
      cmd + ctrl - 0 : yabai -m space --balance

      cmd - 0x2F : elvish ~/dotfiles/bin/__hide_show_alacritty.sh
      cmd + ctrl - 1 : yabai -m window --focus $(yabai -m query --windows | jq '.[] | select(.app=="Emacs") | .id')
      cmd + ctrl - 2 : yabai -m window --focus $(yabai -m query --windows | jq '.[] | select(.app=="Firefox") | .id')

      # make floating window fill screen
      # shift + alt - up     : yabai -m window --grid 1:1:0:0:1:1

      # make floating window fill left-half of screen
      # shift + alt - left   : yabai -m window --grid 1:2:0:0:1:1

      # create desktop, move window and follow focus - uses jq for parsing json (brew install jq)
      # shift + cmd - n : yabai -m space --create && \
      #                   index="$(yabai -m query --spaces --display | jq 'map(select(."native-fullscreen" == 0))[-1].index')" && \
      #                   yabai -m window --space "''${index}" && \
      #                   yabai -m space --focus "''${index}"

      # fast focus desktop
      # cmd + ctrl - x : yabai -m space --focus recent
      # cmd + ctrl - 1 : yabai -m space --focus 1
      # cmd + ctrl - 2 : yabai -m space --focus 2

      # send window to desktop and follow focus
      # shift + cmd - z : yabai -m window --space next; yabai -m space --focus next
      # shift + cmd - 2 : yabai -m window --space  2; yabai -m space --focus 2

      # focus monitor
      cmd + ctrl - k  : yabai -m display --focus prev
      cmd + ctrl - j  : yabai -m display --focus next
      # ctrl + alt - 3  : yabai -m display --focus 3

      # send window to monitor and follow focus
      # ctrl + cmd - c  : yabai -m window --display next; yabai -m display --focus next
      # ctrl + cmd - 1  : yabai -m window --display 1; yabai -m display --focus 1

      # move floating window
      # shift + ctrl - a : yabai -m window --move rel:-20:0
      # shift + ctrl - s : yabai -m window --move rel:0:20

      # increase window size
      # shift + alt - a : yabai -m window --resize left:-20:0
      # shift + alt - w : yabai -m window --resize top:0:-20

      # decrease window size
      # shift + cmd - s : yabai -m window --resize bottom:0:-20
      # shift + cmd - w : yabai -m window --resize top:0:20

      # set insertion point in focused container
      # ctrl + alt - h : yabai -m window --insert west

      # toggle window zoom
      cmd - return : yabai -m window --toggle zoom-parent
      cmd + ctrl - return : yabai -m window --toggle zoom-fullscreen

      # toggle window split type
      cmd + shift - return  : yabai -m window --toggle split

      # float / unfloat window and center on screen
      # alt - t : yabai -m window --toggle float;\
      #           yabai -m window --grid 4:4:1:1:2:2

      # toggle sticky(+float), topmost, picture-in-picture
      # alt - p : yabai -m window --toggle sticky;\
      #           yabai -m window --toggle topmost;\
      #           yabai -m window --toggle pip

      # https://github.com/koekeishiya/yabai/wiki/Tips-and-tricks#quickly-restart-the-yabai-launch-agent
      cmd + shift - c : launchctl kickstart -k "gui/''${UID}/homebrew.mxcl.yabai"
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
      "keybase"
      "todoist"
      "raycast"
      "librewolf"
      "dropbox"
    ];
  };
  # https://github.com/LnL7/nix-darwin/blob/master/modules/security/pam.nix#L25
  security.pam.enableSudoTouchIdAuth = true;

  # Used for backwards compatibility, please read the changelog before changing.
  # $ darwin-rebuild changelog
  system.stateVersion = 4;
  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  nix.package = pkgs.nix;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
}
