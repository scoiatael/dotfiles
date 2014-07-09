set-option -g destroy-unattached on 
set-option -g default-command "reattach-to-user-namespace -l zsh"
set -g terminal-overrides 'xterm*:smcup@:rmcup@'
set -g mouse-utf8 off

#urxvt tab like window switching (-n: no prior escape seq)
bind -n S-down new-window
bind -n S-left prev
bind -n S-right next

set -g mode-mouse on
set -g mouse-resize-pane on
set -g mouse-select-pane on
set -g mouse-select-window on

#source-file ~/config/repos/solarized/tmux/tmuxcolors-dark.conf
source-file ~/config/tmuxln
