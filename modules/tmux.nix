{ config, lib, pkgs, ... }:

{
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
      (callPackage ../packages/tmux-colortag { })
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
  programs.zsh.envExtra = ''
    export TMUX_COLORTAG_TAG_ONLY=yes
    export TMUX_COLORTAG_USE_POWERLINE=yes
    export TMUX_COLORTAG_ROUNDED_POWERLINE=yes
  '';
}
