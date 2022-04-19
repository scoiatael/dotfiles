{ config, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
  programs.git = {
    enable = true;
    aliases = {
      lg = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit";
      lga = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit --all";
      lgd = "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit -p";

      rbc = "rebase --continue";
      rba = "rebase --abort";

      df = "diff --color --color-words --abbrev";
      su = "submodule update --init --recursive";

      ros = "remote set-url origin";
      r = "remote -v";

      bms="! git branch --merged | grep -v '*'";
      bmc="! git branch --merged | grep -v '*' | xargs git branch -d";

      la = "config --get-regexp alias";
      cm = "checkout master";
      cmp ="! git checkout master && git pull --rebase";

      prune = "remote prune origin";

      cb="checkout -b";
      co="checkout";
    };
    ignores = [
      # Source https://github.com/github/gitignore/blob/master/Global/Emacs.gitignore
      "*~"
      "\#*\#"
      "/.emacs.desktop"
      "/.emacs.desktop.lock"
      "/TODO.org"
      "*.elc"
      "auto-save-list"
      "tramp"
      ".\#*"
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
      "^session\.*"

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
    includes = [
      { path = "~/.gitconfig_custom"; }
    ];
    delta.enable = true;
  };

  home.packages = with pkgs; [
    neofetch
    gnutar
    rclone
    procs
    tokei
    du-dust
    fd
    jq
    ripgrep
    python38 # for tmux-colortag
    coreutils-full # for tmux-colortag
    bash # for tmux-colortag
  ];

  programs = {
    starship = {
      enable = true;
      package = (with import <unstable> {}; starship);
      settings = {
        format = ''$cmd_duration$username(\[$git_status\])$character'';
        right_format = ''$git_branch(\[$git_state\])ǂ$directory'';
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
          format = ''\([$state( $progress_current/$progress_total)]($style)\)'';
          style = "bright-black";
        };

        cmd_duration = {
          format = "[  $duration\n]($style)";
          style = "yellow";
        };

      };
    };
    nushell = {
      package = (with import <unstable> {}; nushell); # Need nushell 0.60.0+ for starship
      enable = true;
    };
    dircolors = {
      enable = true;
    };
    bat = {
      enable = true;
    };
    exa = {
      enable = true;
      enableAliases = true;
    };
    broot = {
      enable = true;
    };
    helix = {
      enable = true;
      package = (with import <unstable> {}; helix);
      settings = {
        theme = "monokai_pro_machine";
        keys.normal = {
          space.space = "file_picker";
          space.w = ":w";
          space.q = ":q";
        };
      };
    };
    fzf = {
      enable = true;
    };
    zoxide = {
      enable = true;
    };
    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
      stdlib = "source ~/dotfiles/config/direnvrc";
    };
    emacs = {
      enable = true;
      package = pkgs.emacsNativeComp;
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
        tmuxPlugins.prefix-highlight
      ];
      tmuxinator.enable = true;
      keyMode = "vi";
      escapeTime = 0;
      # it might consume lots of RAM... YES. Please. I'm not using Chrome nor Java, so eat all of it.
      historyLimit = 500000;
      prefix = "C-Space";
    };
  };
  home.file.".envrc".text = ""; # for direnv to load in HOME
  home.file.".tmux/plugins/tmux-colortag".source = builtins.fetchGit { url = "https://github.com/scoiatael/tmux-colortag.git"; };
  xdg.configFile."nu/config.nu".text = # Add "source ~/.config/nu/config.nu" to end of $nu.config-path
    ''
      mkdir ~/.cache/starship
      starship init nu | save ~/.cache/starship/init.nu
      source ~/.cache/starship/init.nu
    '';
}
