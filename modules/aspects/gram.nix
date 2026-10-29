{
  den.aspects.gram = {
    homeManager = {
      # on 26-11:
      # programs.gram = {
      #   enable = true;
      #   # // For information on how to configure Gram, see the Gram
      #   # // documentation: gram://docs/configuring-gram
      #   # //
      #   # // To see all of Gram's default settings without changing your
      #   # // custom settings, run `gram: open default settings` from the
      #   # // command palette (cmd-shift-p / ctrl-shift-p)
      #   settings = {
      #     "buffer_font_family" = "JetBrains Mono NL";
      #     "helix_mode" = false;
      #     "base_keymap" = "VSCode";
      #     "ui_font_size" = 14;
      #     "buffer_font_size" = 14;
      #     "theme" = "Catppuccin Frappé";
      #   };
      # };
      xdg.configFile."gram/settings.jsonc".text = ''
        // Gram settings
        //
        // For information on how to configure Gram, see the Gram
        // documentation: gram://docs/configuring-gram
        //
        // To see all of Gram's default settings without changing your
        // custom settings, run `gram: open default settings` from the
        // command palette (cmd-shift-p / ctrl-shift-p)
        {
          "buffer_font_family": "JetBrains Mono NL",
          "helix_mode": false,
          "base_keymap": "VSCode",
          "ui_font_size": 14,
          "buffer_font_size": 14,
          "theme": "Catppuccin Frappé",
        }
      '';
    };
    darwin = {
      homebrew.casks = [ "gram" ];
    };
  };
}
