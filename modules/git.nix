{ config, lib, pkgs, ... }:

let inherit (lib) mkDefault;
in {
  home.packages = with pkgs; [
    mergiraf
    meld
    gitu
    delta
    tig
    git-revise
    git-branchless
  ];
  programs.git = {
    enable = true;
    lfs.enable = true;
    aliases = {
      bl = "! git-branchless";
      rv = "! git-revise";
      sl = "! git-branchless smartlog";
      d = "diff";
      f = "fetch";
      pp = "! git pull && git push";
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
      "/result"
      "/.secret/"
      "/.org/"
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

      # Asdf-vm
      ".tool-versions"

      # mvbak
      "*.bak.*"
    ];
    extraConfig = {
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
      user = {
        name = mkDefault "Lukasz Czaplinski";
        email = mkDefault "git@scoiatael.dev";
      };
      github.user = "scoiatael";
      status.short = true;
      pull.rebase = true;
      rebase.autoSquash = true;
      init.defaultBranch = "main";
      rerere.enabled = true;
      column.ui = "auto";
      branch.sort = "committerdate";
      merge.mergiraf = {
        name = "mergiraf";
        driver = "mergiraf merge --git %O %A %B -s %S -x %X -y %Y -p %P";
      };
      core.gitattributesfile = "${../config/gitattributes}";
      core.pager = "delta";
      interactive.diffFilter = "delta --color-only";
      delta.navigate = "true"; # use n and N to move between diff sections
      delta.minus-style = "bold red";
      delta.plus-style = "syntax #002800";
      delta.syntax-theme = "catpuccin";
      merge.conflictStyle = "zdiff3";
    };
    signing = {
      signByDefault = true;
      key = mkDefault "EAB800957676ADBE2E29E1B61F748B25B736F0A8";
    };
    # includes = [{ path = "${gitAlias}/gitalias.txt"; }];
    difftastic.enable = true;
    delta.enable = false;
  };

  programs.jujutsu = {
    enable = true;
    settings = {
      ui.diff-editor = "vscode";
      ui.pager = [ "delta" ];
      ui.default-command = "l";
      aliases.la = [ "log" "-r" "(main..@):: | (main..@)-" ];
      aliases.l = [ "log" "-s" "-r" "branches() | main@origin" ];
      user = {
        name = config.programs.git.extraConfig.user.name;
        email = config.programs.git.extraConfig.user.email;
      };
    };
  };

  programs.gh = {
    enable = true;
    extensions = [ pkgs.gh-eco (pkgs.callPackage ../packages/gh-poi { }) ];
    settings = { git_protocol = "ssh"; };
  };

  programs.gh-dash = { enable = true; };
}
