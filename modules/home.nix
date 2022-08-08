{ config, lib, pkgs, emacs-overlay, ... }:

{
  nixpkgs.overlays = [ emacs-overlay.overlay ];
  programs.gpg = { enable = true; };
  programs.git = {
    enable = true;
    lfs.enable = true;
    aliases = {
      lg =
        "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      lga =
        "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --all";
      lgd =
        "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit -p";

      rbc = "rebase --continue";
      rba = "rebase --abort";

      df = "diff --color --color-words --abbrev";
      su = "submodule update --init --recursive";

      ros = "remote set-url origin";
      r = "remote -v";

      bms = "! git branch --merged | grep -v '*'";
      bmc = "! git branch --merged | grep -v '*' | xargs git branch -d";

      la = "config --get-regexp alias";
      cm = "checkout master";
      cmp = "! git checkout master && git pull --rebase";

      prune = "remote prune origin";

      cb = "checkout -b";
      co = "checkout";

      s = "status";
      search = "!git rev-list --all | xargs git grep -F";
      last = "log -1 HEAD --stat";
      continue = "!$HOME/dotfiles/bin/__git_continue.sh";
      br =
        "branch --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(contents:subject) %(color:green)(%(committerdate:relative)) [%(authorname)]' --sort=-committerdate";
      l1 = "rev-parse --short HEAD";
    };
    ignores = [
      # Source https://github.com/github/gitignore/blob/master/Global/Emacs.gitignore
      "*~"
      "#*#"
      "/.emacs.desktop"
      "/.emacs.desktop.lock"
      "/TODO.org"
      "*.elc"
      "auto-save-list"
      "tramp"
      ".#*"
      "recentf"
      "srecode-map.el"
      ".dir-locals.el"
      ".project"
      ".projectile"

      # Org-mode
      ".org-id-locations"
      "*_archive"

      # flymake-mode
      "*_flymake.*"

      # eshell files
      "/eshell/history"
      "/eshell/lastdir"

      # elpa packages
      "/elpa/"

      # reftex files
      "*.rel"

      # AUCTeX auto folder
      "/auto/"

      # cask packages
      ".cask/"

      # emacs sessions
      "^session.*"

      # ctags
      "TAGS"

      # Source https://github.com/github/gitignore/blob/master/Global/Vim.gitignore
      "[._]*.s[a-w][a-z]"
      "[._]s[a-w][a-z]"
      "*.un~"
      "Session.vim"
      ".netrwhist"
      "*~"

      # JetBrains
      # Source https://raw.githubusercontent.com/github/gitignore/master/Global/JetBrains.gitignore
      # covers JetBrains IDEs: IntelliJ, RubyMine, PhpStorm, AppCode, PyCharm, CLion, Android Studio and Webstorm
      # Reference: https://intellij-support.jetbrains.com/hc/en-us/articles/206544839
      ".idea/"

      # User-specific stuff:
      ".idea/workspace.xml"
      ".idea/tasks.xml"
      ".idea/dictionaries"
      ".idea/vcs.xml"
      ".idea/jsLibraryMappings.xml"

      # Sensitive or high-churn files:
      ".idea/dataSources.ids"
      ".idea/dataSources.xml"
      ".idea/dataSources.local.xml"
      ".idea/sqlDataSources.xml"
      ".idea/dynamic.xml"
      ".idea/uiDesigner.xml"

      # Gradle:
      ".idea/gradle.xml"
      ".idea/libraries"

      # Mongo Explorer plugin:
      ".idea/mongoSettings.xml"

      ## File-based project format:
      "*.iws"

      ## Plugin-specific files:

      # IntelliJ
      "/out/"

      # mpeltonen/sbt-idea plugin
      ".idea_modules/"

      # JIRA plugin
      "atlassian-ide-plugin.xml"

      # Crashlytics plugin (for Android Studio and IntelliJ)
      "com_crashlytics_export_strings.xml"
      "crashlytics.properties"
      "crashlytics-build.properties"
      "fabric.properties"

      # OS X
      ".DS_Store"

      # GTags
      "GPATH"
      "GTAGS"
      "GRTAGS"

      # Virtualfish
      ".venv"

      # Ensime for Scala
      ".ensime"
      ".ensime_cache"

      # direnv
      ".direnv/"

      # Visual Studio Code ElixirLS
      ".elixir_ls"

      # Direnv
      ".envrc"
      ".envrc.enc*"

      # Asdf-vm
      ".tool-versions"
    ];
    extraConfig = {
      core.editor = "edit";
      color = {
        branch = "auto";
        diff = "auto";
        interactive = "auto";
        status = "auto";
      };
      push.default = "current";
      magit.hideCampaign = true;
      diff.sopsdiffer.textconv = "sops -d";
      magithub = {
        online = false;
        status = {
          includeStatusHeader = false;
          includePullRequestsSection = false;
          includeIssuesSection = false;
        };
      };
      github.user = "scoiatael";
      status.short = true;
      pull.rebase = true;
      rebase.autoSquash = true;
      init.defaultBranch = "master";
    };
    signing = {
      signByDefault = true;
      key = "EAB800957676ADBE2E29E1B61F748B25B736F0A8";
    };
    includes = [{ path = "~/.gitconfig_custom"; }];
    delta.enable = true;
  };

  home.packages = with pkgs; [
    neofetch
    gnutar
    rclone
    tokei
    du-dust
    fd
    jq
    ripgrep
    python38 # for tmux-colortag
    coreutils-full # for tmux-colortag
    bash # for tmux-colortag
    fpp # for tmux-fpp
    units
    tig
    lsof
    ruby_3_0
    meld
    sops
    viddy
    recutils
    aspell
    aspellDicts.pl
    aspellDicts.cs
    aspellDicts.en
    aspellDicts.en-science
    aspellDicts.en-computers
    ispell
    hyperfine
    nixfmt
    file
    imagemagick
  ];

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
      stdlib = "source ~/dotfiles/config/direnvrc";
    };
    emacs = {
      enable = true;
      package = pkgs.emacsPgtkNativeComp;
    };
    tmux = {
      enable = true;
      extraConfig = "source-file ~/dotfiles/config/tmux.conf";
      plugins = with pkgs; [
        tmuxPlugins.yank
        tmuxPlugins.jump
        tmuxPlugins.sensible
        tmuxPlugins.extrakto
        tmuxPlugins.tmux-fzf
        tmuxPlugins.open
        {
          plugin = tmuxPlugins.prefix-highlight;
          extraConfig = ''
            set-option -g status-right '#{prefix_highlight} #(echo "#{pane_title}" | cut -d" " -f1) #[fg=red,dim,bg=default]#(ps -o command -g "#{pane_pid}" | grep "ssh " | cut -d" " -f2-)#[fg=default,nodim,bg=default]#{?window_bigger,[#{window_offset_x}#,#{window_offset_y}] ,}'
            set-option -g status-right-length 80
            set-option -g status-position bottom
          '';
        }
        {
          plugin = tmuxPlugins.fpp;
          extraConfig = "set -g @fpp-key 'X'";
        }
      ];
      tmuxinator.enable = true;
      keyMode = "vi";
      escapeTime = 0;
      # it might consume lots of RAM... YES. Please. I'm not using Chrome nor Java, so eat all of it.
      historyLimit = 500000;
      prefix = "C-Space";
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
    ~/dotfiles/config/direnvrc; # for direnv to load in HOME

  home.file.".tmux/plugins/tmux-colortag".source = builtins.fetchGit {
    url = "https://github.com/scoiatael/tmux-colortag.git";
  };

  xdg.configFile."nu/config.nu".text = # Add "source ~/.config/nu/config.nu" to end of $nu.config-path
    ''
      mkdir ~/.cache/starship
      starship init nu | save ~/.cache/starship/init.nu
      source ~/.cache/starship/init.nu
    '';

  home.file.".elvish/rc.elv".source = ~/dotfiles/config/elvish/rc.elv;
  home.file.".elvish/lib/direnv.elv".source =
    ~/dotfiles/config/elvish/lib/direnv.elv;
  home.file.".elvish/lib/zoxide.elv".source =
    ~/dotfiles/config/elvish/lib/zoxide.elv;
  home.file.".elvish/lib/starship.elv".source =
    ~/dotfiles/config/elvish/lib/starship.elv;

  home.file.".config/doom".source = ~/dotfiles/config/doom;
  home.file.".emacs.doom".source =
    builtins.fetchGit { url = "https://github.com/doomemacs/doomemacs.git"; };
  home.file.".emacs.d/early-init.el".text = ''
    (setenv "DOOMLOCALDIR" (expand-file-name (file-name-as-directory "~/.emacs.local/")))
    (setenv "LSP_USE_PLISTS" "true")
    (setq user-emacs-directory (expand-file-name (file-name-as-directory "~/.emacs.doom/")))
    (load (concat user-emacs-directory "early-init.el") nil 'nomessage)
  '';

  home.file.".zprofile".text = ''
    export NIX_PATH=$HOME/.nix-defexpr/channels:/nix/var/nix/profiles/per-user/root/channels
    export DOOMLOCALDIR="~/.emacs.local"
    export EMACSDIR="~/.emacs.doom"
  '';
}
