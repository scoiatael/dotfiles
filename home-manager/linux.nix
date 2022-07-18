{ config, lib, pkgs, ... }:

{
    services.gpg-agent = {
        enable = true;
        enableSshSupport = true;
        pinentryFlavor = "qt";
    };
    services.syncthing = {
        enable = true;
        # tray = {
        #     enable = true;
        #     package = pkgs.qsyncthingtray;
        #     command = "qsyncthingtray --wait";
        #   }; doesn't work due to missing 'tray.target'
    };
    services.emacs = {
        enable = true;
    };
    home.packages = with pkgs; [
        janet # broken on macOS
        procs # broken on macOS
        libnotify # doesn't work on macOS
        lm_sensors # for temp display in i3status-rust?
    ] ++ (with pkgs; [
        (nerdfonts.override { fonts = [
                                  "JetBrainsMono"  # needed for rofi theme
                              ]; })
    ]);
    fonts.fontconfig.enable = true; # required to autoload fonts from packages

    programs.qutebrowser.enable = true;
    wayland.windowManager.sway.swaynag = {
        enable = true;
    };
    wayland.windowManager.sway = {
        enable = true;
        package = null;
        wrapperFeatures = {
            gtk = true;
            base = true;
        };
        extraConfig = ''
    ### Output configuration
    #
    # Default wallpaper (more resolutions are available in /run/current-system/sw/share/backgrounds/sway/)
    output * bg /run/current-system/sw/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png fill
    #
    # Example configuration:
    #
    #   output HDMI-A-1 resolution 1920x1080 position 1920,0
    #
    # You can get the names of your outputs by running: swaymsg -t get_outputs
    output  'Acer Technologies XB271HK #ASOP6fQBuwTd' scale 1.8
    output 'Unknown 0x095F 0x00000000' scale 1.8

    ### Idle configuration
    #
    # Example configuration:
    #
    exec swayidle -w \
            timeout 300 'swaylock -f -c 000000' \
            timeout 600 'swaymsg "output * dpms off"' resume 'swaymsg "output * dpms on"' \
            before-sleep 'swaylock -f -c 000000'
    #
    # This will lock your screen after 300 seconds of inactivity, then turn off
    # your displays after another 300 seconds, and turn your screens back on when
    # resumed. It will also lock your screen before your computer goes to sleep.

    input "type:touchpad" {
        dwt enabled
        tap enabled
        natural_scroll enabled
        middle_emulation enabled
    }
    #
    input "type:keyboard" {
        xkb_layout pl
        xkb_variant ,nodeadkeys
        xkb_options caps:swapescape
    }

    # https://dev.gnupg.org/T6041
    for_window [app_id="pinentry-qt"] floating enable

    include /etc/sway/config.d/*
    '';
        config = {
            modifier = "Mod4";
            menu = "rofi -no-lazy-grab -show drun";
            bars =  [{
                mode = "dock";
                hiddenState = "hide";
                position = "top";
                workspaceButtons = true;
                workspaceNumbers = true;
                statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ~/.config/i3status-rust/config-default.toml";
                fonts = {
                    names = [ "JetBrainsMono Nerd Font" ];
                    size = 9.0;
                };
                trayOutput = "primary";
                colors = {
                    background = "#323232";
                    statusline = "#ffffff";
                    separator = "#666666";
                    focusedWorkspace = {
                        border = "#4c7899";
                        background = "#285577";
                        text = "#ffffff";
                    };
                    activeWorkspace = {
                        border = "#333333";
                        background = "#5f676a";
                        text = "#ffffff";
                    };
                    inactiveWorkspace = {
                        border = "#333333";
                        background = "#222222";
                        text = "#888888";
                    };
                    urgentWorkspace = {
                        border = "#2f343a";
                        background = "#900000";
                        text = "#ffffff";
                    };
                    bindingMode = {
                        border = "#2f343a";
                        background = "#900000";
                        text = "#ffffff";
                    };
                };
            }];
        };
    };
    programs.mako = {
        enable = true;
        font = "JetBrainsMono Nerd Font 10";
        backgroundColor = "#1E1D2F";
        textColor = "#D9E0EE";
        defaultTimeout = 5000;
    };
    programs.waybar = {
        enable = true;
    };
    programs.i3status-rust = {
        enable = true;
        bars = {
            default = {
                settings = {
                    icons = { name = "material-nf"; };
                    theme = { name = "slick"; };
                    block = [
                        { block = "uptime"; }
                        {
                            block = "disk_space";
                            path = "/";
                            alias = "/";
                            info_type = "available";
                            unit = "GB";
                            interval = 60;
                            warning = 20.0;
                            alert = 10.0;
                        }
                        {
                            block = "memory";
                            display_type = "memory";
                            format_mem = "{mem_used_percents}";
                            format_swap = "{swap_used_percents}";
                        }
                        {
                            block = "cpu";
                            interval = 1;
                        }
                        {
                            block = "temperature";
                            collapsed = true;
                            interval = 10;
                            format = "{min} min, {max} max, {average} avg";
                        }
                        {
                            block = "load";
                            interval = 1;
                            format = "{1m}";
                        }
                        { block = "sound"; }
                        {
                            block = "battery";
                            interval = 10;
                            format = "{percentage} {time}";
                        }
                        {
                            block = "backlight";
                            minimum = 15;
                            maximum = 100;
                            cycle = [100 75 50 25 0 25 50 75];
                        }
                        {
                            block = "networkmanager";
                            on_click = "alacritty -e nmtui";
                            interface_name_exclude = ["br\\-[0-9a-f]{12}"  "docker\\d+"];
                            interface_name_include = [];
                            ap_format = "{ssid^10}";
                        }
                        {
                            block = "time";
                            interval = 60;
                            format = "%a %d/%m %R";
                        }
                    ];
                };
            };
        };
    };
    programs.rofi =      let
        # Use `mkLiteral` for string-like values that should show without
        # quotes, e.g.:
        # {
        #   foo = "abc"; => foo: "abc";
        #   bar = mkLiteral "abc"; => bar: abc;
        # };
        inherit (config.lib.formats.rasi) mkLiteral;
    in {
        enable = true;
        extraConfig = {
            modi = "drun,ssh,windowcd,combi,keys,filebrowser";
            lines = 5;
            font = "JetBrainsMono Nerd Font 14";
            show-icons = true;
            icon-theme = "Oranchelo";
            drun-display-format = "{icon} {name}";
            location = 0;
            disable-history = false;
            hide-scrollbar = true;
            display-drun = "   Apps ";
            display-run = "   Run ";
            display-window = " 﩯  Window";
            display-Network = " 󰤨  Network";
            sidebar-mode = true;
        };
        theme = {
            "*" = {
                bg-col = mkLiteral "#1E1D2F";
                bg-col-light = mkLiteral "#1E1D2F";
                border-color = mkLiteral "#1E1D2F";
                selected-col = mkLiteral "#1E1D2F";
                blue = mkLiteral "#7aa2f7";
                grey = mkLiteral "#D9E0EE";
                fg-col = mkLiteral "#D9E0EE";
                fg-col2 = mkLiteral "#F28FAD";
                width = 600;
            };
            "element-text, element-icon, mode-switcher" =  {
                background-color = mkLiteral "inherit";
                text-color = mkLiteral "inherit";
            };

            window = {
                height = mkLiteral "360px";
                border = mkLiteral "3px";
                border-color = mkLiteral "@border-col";
                background-color = mkLiteral "@bg-col";
            };

            mainbox = {
                background-color = mkLiteral "@bg-col";
            };

            inputbar = {
                children = mkLiteral "[prompt,entry]";
                background-color = mkLiteral "@bg-col";
                border-radius = mkLiteral "5px";
                padding = mkLiteral "2px";
            };

            prompt = {
                background-color = mkLiteral "@blue";
                padding = mkLiteral "6px";
                text-color = mkLiteral "@bg-col";
                border-radius = mkLiteral "3px";
                margin = mkLiteral "20px 0px 0px 20px";
            };

            textbox-prompt-colon = {
                expand = mkLiteral "false";
                str = ":";
            };

            entry = {
                padding = mkLiteral "6px";
                margin = mkLiteral "20px 0px 0px 10px";
                text-color = mkLiteral "@fg-col";
                background-color = mkLiteral "@bg-col";
            };

            listview = {
                border = mkLiteral "0px 0px 0px";
                padding = mkLiteral "6px 0px 0px";
                margin = mkLiteral "10px 0px 0px 20px";
                columns = mkLiteral "2";
                background-color = mkLiteral "@bg-col";
            };

            element = {
                padding = mkLiteral "5px";
                background-color = mkLiteral "@bg-col";
                text-color = mkLiteral "@fg-col";
            };

            element-icon = {
                size = mkLiteral "25px";
            };

            "element selected" = {
                background-color = mkLiteral " @selected-col";
                text-color = mkLiteral "@fg-col2";
            };

            mode-switcher = {
                spacing = mkLiteral "0";
            };

            button = {
                padding = mkLiteral "10px";
                background-color = mkLiteral "@bg-col-light";
                text-color = mkLiteral "@grey";
                vertical-align = mkLiteral "0.5";
                horizontal-align = mkLiteral "0.5";
            };

            "button selected" = {
                background-color = mkLiteral "@bg-col";
                text-color = mkLiteral "@blue";
            };
        };
        terminal = "${pkgs.alacritty}/bin/alacritty";
    };
}
