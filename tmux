set-option -g destroy-unattached off
set -g terminal-overrides 'xterm*:smcup@:rmcup@'
set -g mouse-utf8 off
set -g escape-time 0
set -g repeat-time 0

unbind C-b
set -g prefix C-q

#urxvt tab like window switching (-n: no prior escape seq)
bind -n S-down new-window
bind -n S-left prev
bind -n S-right next

set -g mode-mouse on
set -g mouse-resize-pane on
set -g mouse-select-pane on
set -g mouse-select-window on

#switch panels
bind k selectp -U # switch to panel Up
bind j selectp -D # switch to panel Down
bind h selectp -L # switch to panel Left
bind l selectp -R # switch to panel Right

bind-key -r C-h select-window -t :-
bind-key -r C-l select-window -t :+

##CLIPBOARD selection integration
##Requires prefix key before the command key
##Copy tmux paste buffer to CLIPBOARD
bind C-c run "tmux show-buffer | xsel -i -b"
##Copy CLIPBOARD to tmux paste buffer and paste tmux paste buffer
bind C-v run "tmux set-buffer -- \"$(xsel -o -b)\"; tmux paste-buffer"

#source-file ~/config/repos/solarized/tmux/tmuxcolors-dark.conf
source-file ~/config/tmuxln
