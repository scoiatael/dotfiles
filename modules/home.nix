{ lib, pkgs, ... }: {
  programs.gpg = { enable = true; };
  programs.zellij = {
    enable = true;
    settings = { theme = "catppuccin-macchiato"; };
  };
  home.file.".config/zellij/themes/catppuccin.kdl".text = ''
        // Catppuccin Theme:
    // https://github.com/catppuccin/catppuccin

    themes {
      catppuccin-latte {
        bg "#acb0be" // Surface2
        fg "#5c5f77" // Subtext1
        red "#d20f39"
        green "#40a02b"
        blue "#1e66f5"
        yellow "#df8e1d"
        magenta "#ea76cb" // Pink
        orange "#fe640b" // Peach
        cyan "#04a5e5" // Sky
        black "#dce0e8" // Crust
        white "#4c4f69" // Text
      }

      catppuccin-frappe {
        bg "#626880" // Surface2
        fg "#c6d0f5"
        red "#e78284"
        green "#a6d189"
        blue "#8caaee"
        yellow "#e5c890"
        magenta "#f4b8e4" // Pink
        orange "#ef9f76" // Peach
        cyan "#99d1db" // Sky
        black "#292c3c" // Mantle
        white "#c6d0f5"
      }

      catppuccin-macchiato {
        bg "#5b6078" // Surface2
        fg "#cad3f5"
        red "#ed8796"
        green "#a6da95"
        blue "#8aadf4"
        yellow "#eed49f"
        magenta "#f5bde6" // Pink
        orange "#f5a97f" // Peach
        cyan "#91d7e3" // Sky
        black "#1e2030" // Mantle
        white "#cad3f5"
      }

      catppuccin-mocha {
        bg "#585b70" // Surface2
        fg "#cdd6f4"
        red "#f38ba8"
        green "#a6e3a1"
        blue "#89b4fa"
        yellow "#f9e2af"
        magenta "#f5c2e7" // Pink
        orange "#fab387" // Peach
        cyan "#89dceb" // Sky
        black "#181825" // Mantle
        white "#cdd6f4"
      }
    }
  '';

  home.language = { base = "en_GB.UTF-8"; };

  home.packages = with pkgs; [
    elvish # until I port scripts back..
    patchutils
    html-tidy
    wget
    neofetch
    curlie
    xh
    dive
    gnutar
    tokei
    du-dust
    fd
    jq
    ripgrep
    units
    lsof
    ruby_3_3
    sops
    viddy
    hyperfine
    file
    imagemagick
    yarn # for yarn create
    unzip
    zip
    bind # for dig
    pwgen
    entr
    yubikey-manager
    pry
    restic
    rsync
    mu
    stylua
    (callPackage ../packages/gitblame { })
    (callPackage ../packages/inflector-rs { })
    (callPackage ../packages/indices { })
    # (callPackage ../packages/ternimal { })
    # janet
    # luakit # doesn't work on aarch64-darwin :/
    devenv
    nixd

    jetbrains-mono
    dejavu_fonts
    noto-fonts-color-emoji
    unifont
    udev-gothic
  ];

  fonts.fontconfig.enable = true; # required to autoload fonts from packages

  programs = {
    alacritty = {
      enable = true;
      settings = {
        font = {
          normal = { family = "JetBrainsMono Nerd Font"; };
          size = 12;
        };
        terminal = { shell = { program = "${pkgs.zsh}/bin/zsh"; }; };
        window.decorations = "None";
        # import =
        #  [ "${config.home.homeDirectory}/.config/alacritty/theme.toml" ];
      };
    };
    qutebrowser = {
      # enable = true; broken on macOS -> enable via ./home-manager/linux.nix
      searchEngines = {
        w =
          "https://en.wikipedia.org/wiki/Special:Search?search={}&go=Go&ns0=1";
        aw = "https://wiki.archlinux.org/?search={}";
        nw = "https://nixos.wiki/index.php?search={}";
        g = "https://www.google.com/search?hl=en&q={}";
        nix = "https://search.nixos.org/packages?query={}";
        k = "https://kagi.com/search?q={}";
        DEFAULT = "https://kagi.com/search?q={}";
        m = "https://melpa.org/#/?q={}";
        b = "https://search.brave.com/search?q={}";
      };
      settings = {
        url.start_pages = "https://kagi.com";
        url.default_page = "https://kagi.com";
        # colors.hints.bg = "qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 rgb(255, 247, 133), stop:1 rgb(255, 197, 66))";
        # colors.webpage.darkmode.enabled = true;
        content.cookies.accept = "no-3rdparty";
        content.default_encoding = "utf-8";
        hints.uppercase = true;
        zoom.default = "135%";
      };
    };
    starship = {
      enable = true;
      enableNushellIntegration = false; # broken as of 2022/12/03
      settings = {
        format = ''
          $cmd_duration┌$fill$git_branch$git_stateǂ$directory
          └$username$git_status$character'';

        command_timeout = 90;

        fill = {
          symbol = "─";
          style = "bold green";
        };

        directory = {
          style = "blue";
          truncate_to_repo = false;
        };

        character = {
          success_symbol = "[λ](purple)";
          error_symbol = "[λ](red)";
          vicmd_symbol = "[❮](green)";
        };

        git_branch = {
          format = "[$branch]($style)";
          style = "bright-black";
        };

        git_status = {
          format = "[\\($ahead_behind$staged$modified\\)]($style)";
          style = "cyan";
          conflicted = "[䷅](red)";
          untracked = "[·](grey)";
          modified = "[·](yellow)";
          stashed = ""; # not interesting, keep empty
          staged = "[·](green)";
        };

        git_state = {
          format =
            "\\([|$state( $progress_current/$progress_total)]($style)\\)";
          style = "bright-black";
        };

        cmd_duration = {
          format = ''
            [  $duration
            ]($style)'';
          style = "yellow";
        };
      };
    };
    dircolors = { enable = true; };
    bat = {
      enable = true;
      themes = {
        catpuccin = {
          src = pkgs.fetchFromGitHub {
            owner = "catppuccin";
            repo = "bat";
            rev = "d2bbee4f7e7d5bac63c054e4d8eca57954b31471";
            sha256 = "sha256-x1yqPCWuoBSx/cI94eA+AWwhiSA42cLNUOFJl7qjhmw=";
          };
          file = "themes/Catppuccin Frappe.tmTheme";
        };
      };
      config = {
        paging = "never";
        theme = "catpuccin";
      };
      extraPackages = with pkgs.bat-extras; [ batman ];
    };
    btop = { enable = true; };
    atuin = {
      enable = true;
      flags = [ "--disable-up-arrow" ];
    };
    eza = { enable = true; };
    broot = { enable = true; };
    helix = {
      enable = true;
      settings = {
        theme = "monokai_pro_machine";
        keys.normal = {
          space.space = "file_picker";
          space.w = ":w";
          space.q = ":q";
        };
      };
    };
    fzf = { enable = true; };
    zoxide = { enable = true; };
    direnv = {
      enable = true;
      nix-direnv = { enable = true; };
      stdlib = "source ${../config/direnvrc}";
    };
    vscode = {
      enable = true;
      package = pkgs.vscodium;
      userSettings = {
        "update.mode" = "none";
        "[nix]"."editor.tabSize" = 2;
        "workbench.startupEditor" = "none";
        "editor.fontFamily" = "JetBrainsMono Nerd Font";
        "editor.fontSize" = 12;
        "workbench.colorTheme" = "GitHub Dark Dimmed";
        "[typescript]"."editor.defaultFormatter" = "esbenp.prettier-vscode";
        "extensions.autoUpdate" = false;
      };
      extensions = with pkgs.vscode-extensions; [
        bbenoist.nix
        github.github-vscode-theme
        esbenp.prettier-vscode
        mattn.lisp
        tamasfe.even-better-toml
        kamadorueda.alejandra
      ];
      mutableExtensionsDir = false;
    };
  };

  home.file.".envrc".text = ""; # for direnv to load in HOME
  home.file.".direnvrc".source =
    ../config/direnvrc; # for direnv to load in HOME

  # home.file.".elvish/rc.elv".source = ../config/elvish/rc.elv;
  # home.file.".elvish/lib/direnv.elv".source = ../config/elvish/lib/direnv.elv;
  # home.file.".elvish/lib/zoxide.elv".source = ../config/elvish/lib/zoxide.elv;
  # home.file.".elvish/lib/starship.elv".source =
  #   ../config/elvish/lib/starship.elv;

  home.file.".zprofile".text = ''
    export DOOMLOCALDIR="~/.emacs.local"
    export EMACSDIR="~/.emacs.doom"
  '';

  home.file.".pryrc".text = ''
    Pry::Prompt.add(
      :vterm,
      "A simple `>>` w/ vterm ending.",
      ['>> ', ' | ']
    ) do |_, _, _, sep|
      whoami = ENV['USER']
      pwd = ENV['pwd']
      sep + "\e]51;A#{whoami}@:#{pwd}\e\\"
    end

    Pry.config.prompt = Pry::Prompt[:vterm]
  '';

  home.file.".gnupg/dirmngr.conf".text = ''
    keyserver hkps://keys.openpgp.org/
  '';

  home.file.".gnupg/gpg-agent.conf" =
    let program = "/run/current-system/sw/bin/pinentry-mac";
    in lib.mkIf pkgs.stdenv.isDarwin { text = "pinentry-program ${program}"; };
}
