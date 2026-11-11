{ inputs, ... }:
{
  flake-file.inputs = {
    nix-gram-extensions.url = "git+https://codeberg.org/niklaskorz/nix-gram-extensions";
    nix-gram-extensions.inputs.nixpkgs.follows = "wasip2-nixpkgs";
    # The default.nix by default fetches a version of nixpkgs that includes the WASIp2 changes required to build Gram extensions from source. You can optionally pass your own instance of pkgs instead, but are then responsible for ensuring this includes the wasm32-wasip2 cross target, which has not yet landed in any upstream nixpkgs channel. You can even pass it your own WASIp2 cross pkgs instance as wasip2Pkgs. From default.nix, you get a set of extensions defined in this repository along with three useful functions buildGramExtension, buildGramRustExtension and linkGramExtensions. For convenience, these are also exposed by a flake.nix.
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
          }
        '';
        home.file."Library/Application Support/Gram/extensions/installed" = {
          enable = pkgs.stdenv.hostPlatform.isDarwin;
          source = extensions-dir;
          onChange = ''
            cd "Library/Application Support/Gram/extensions"
            mv index.json index.json.backup
          '';
        };
        xdg.dataFile."gram/extensions/installed" = {
          enable = pkgs.stdenv.hostPlatform.isLinux;
          source = extensions-dir;
          onChange = ''
            cd "${config.xdg.dataHome}/gram/extensions"
            mv index.json index.json.backup
          '';
        };
      };
    darwin = {
      homebrew.casks = [ "gram" ];
    };
  };
}
