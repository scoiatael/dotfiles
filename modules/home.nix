{ config, lib, pkgs, ... }:

{
  programs.gpg = { enable = true; };

  home.packages = with pkgs; [
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
    (nerdfonts.override {
      fonts = [
        "JetBrainsMono" # needed for rofi theme
        "FiraCode"
      ];
    })
  ];
  fonts.fontconfig.enable = true; # required to autoload fonts from packages

  programs = {
    alacritty = {
      enable = true;
      settings = {
        font = {
          normal = { family = "JetBrainsMono Nerd Font"; };
          size = 9;
        };
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
    nushell = { enable = true; };
    dircolors = { enable = true; };
    bat = { enable = true; };
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
      };
    };
  };

  home.file.".envrc".text = ""; # for direnv to load in HOME
  home.file.".direnvrc".source =
    ../config/direnvrc; # for direnv to load in HOME

  xdg.configFile."nu/config.nu".text = # Add "source ~/.config/nu/config.nu" to end of $nu.config-path
    ''
      mkdir ~/.cache/starship
      starship init nu | save ~/.cache/starship/init.nu
      source ~/.cache/starship/init.nu
    '';

  home.file.".elvish/rc.elv".source = ../config/elvish/rc.elv;
  home.file.".elvish/lib/direnv.elv".source = ../config/elvish/lib/direnv.elv;
  home.file.".elvish/lib/zoxide.elv".source = ../config/elvish/lib/zoxide.elv;
  home.file.".elvish/lib/starship.elv".source =
    ../config/elvish/lib/starship.elv;

  home.file.".zprofile".text = ''
    export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels
    export DOOMLOCALDIR="~/.emacs.local"
    export EMACSDIR="~/.emacs.doom"
  '';
}
