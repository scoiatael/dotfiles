{ config, lib, pkgs, ... }:

let
  tmux-colortag = builtins.fetchGit {
    url = "https://github.com/scoiatael/tmux-colortag.git";
    rev = "c31e5e710d0073534b3f25b6c28a739db5d7630a";
  };
in {
  home.packages = with pkgs; [
    python38 # for tmux-colortag
    coreutils-full # for tmux-colortag
    bash # for tmux-colortag
    fpp # for tmux-fpp
  ];

  programs.tmux = {
    enable = true;
    extraConfig = "source-file ${../config/tmux.conf}";
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

  home.file.".tmux/plugins/tmux-colortag".source = tmux-colortag;
}
