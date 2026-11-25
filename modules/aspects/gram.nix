{ inputs, ... }:
{
  flake-file.inputs = {
    nix-gram-extensions.url = "git+https://codeberg.org/niklaskorz/nix-gram-extensions";
    nix-gram-extensions.inputs.nixpkgs.follows = "wasip2-nixpkgs";
    # Building extensions needs the wasm32-wasip2 target, which no upstream nixpkgs channel has yet.
    wasip2-nixpkgs.url = "github:niklaskorz/nixpkgs/wasip2-rustc";
  };
  den.aspects.gram = {
    homeManager =
      {
        pkgs,
        config,
        system,
        self',
        ...
      }:
      let
        gram-extensions = inputs.nix-gram-extensions.packages.${system};
        extensions = with gram-extensions; [
          catppuccin
          catppuccin-icons
          git-firefly
          self'.packages.clojure-gram
          self'.packages.ruby-gram
          self'.packages.prisma-gram
        ];
        extensions-dir = gram-extensions.linkGramExtensions extensions;
      in
      {

        home.packages = with pkgs; [
          yaml-language-server
          vscode-json-languageserver
          bash-language-server
        ];
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
            "autosave": "on_focus_change",
          }
        '';
        home.file."Library/Application Support/Gram/extensions/installed" = {
          enable = pkgs.stdenv.hostPlatform.isDarwin;
          source = extensions-dir;
          # Gram rebuilds its extension index on launch when the file is gone.
          onChange = ''
            rm -f "${config.home.homeDirectory}/Library/Application Support/Gram/extensions/index.json"
          '';
        };
        xdg.dataFile."gram/extensions/installed" = {
          enable = pkgs.stdenv.hostPlatform.isLinux;
          source = extensions-dir;
          onChange = ''
            rm -f "${config.xdg.dataHome}/gram/extensions/index.json"
          '';
        };
      };
    darwin = {
      homebrew.casks = [ "gram" ];
    };
  };
}
