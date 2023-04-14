{ config, lib, pkgs, ... }:

{
  programs.gpg = { enable = true; };

  home.packages = with pkgs; [
    elvish # until I port scripts back..
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
    tig
    lsof
    ruby_3_0
    meld
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
    rnix-lsp # LSP for nix?!
    yubikey-manager
    pry
    (nerdfonts.override {
      fonts = [
        "JetBrainsMono" # needed for rofi theme
        "FiraCode"
      ];
    })
    # (callPackage ../packages/ternimal { })
    janet
    # luakit # doesn't work on aarch64-darwin :/
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
        colors.primary.background = "0x282828";
        colors.primary.foreground = "0xeeeeee";
        colors.normal.black = "0x282828";
        colors.normal.red = "0xf43753";
        colors.normal.green = "0xc9d05c";
        colors.normal.yellow = "0xffc24b";
        colors.normal.blue = "0xb3deef";
        colors.normal.magenta = "0xd3b987";
        colors.normal.cyan = "0x73cef4";
        colors.normal.white = "0xeeeeee";
        colors.bright.black = "0x4c4c4c";
        colors.bright.red = "0xf43753";
        colors.bright.green = "0xc9d05c";
        colors.bright.yellow = "0xffc24b";
        colors.bright.blue = "0xb3deef";
        colors.bright.magenta = "0xd3b987";
        colors.bright.cyan = "0x73cef4";
        colors.bright.white = "0xfeffff";
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
        format = "$cmd_duration$username(\\[$git_status\\])$character";
        right_format = "$git_branch(\\[$git_state\\])ǂ$directory";
        command_timeout = 90;

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
          format = "[$ahead_behind$staged$modified]($style)";
          style = "cyan";
          conflicted = "[䷅](red)";
          untracked = "[·](grey)";
          modified = "[·](yellow)";
          stashed = ""; # not interesting, keep empty
          staged = "[·](green)";
        };

        git_state = {
          format = "\\([$state( $progress_current/$progress_total)]($style)\\)";
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
    bat = { enable = true; };
    btop = { enable = true; };
    atuin = {
      enable = true;
      flags = [ "--disable-up-arrow" ];
    };
    gh = {
      enable = true;
      extensions = [ pkgs.gh-eco (pkgs.callPackage ../packages/gh-poi { }) ];
      settings = { git_protocol = "ssh"; };
    };
    exa = {
      enable = true;
      enableAliases = true;
    };
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
        "update.channel" = "none";
        "[nix]"."editor.tabSize" = 2;
        "workbench.startupEditor" = "none";
        "editor.fontFamily" = "JetBrainsMono Nerd Font";
        "editor.fontSize" = 12;
        "workbench.colorTheme" = "GitHub Dark Dimmed";
        "[typescript]"."editor.defaultFormatter" = "esbenp.prettier-vscode";
      };
      extensions = [
        pkgs.vscode-extensions.bbenoist.nix
        pkgs.vscode-extensions.github.github-vscode-theme
        pkgs.vscode-extensions.esbenp.prettier-vscode
        pkgs.vscode-extensions.mattn.lisp
      ];
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
    export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels
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
  home.file.".config/karabiner/karabiner.json".source =
    ../config/karabiner.json;
}
