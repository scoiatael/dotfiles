function random
    set -l length $argv[1]
    if test -z $length
        set -l length 10
    end
    ruby -e "puts (('a'..'z').to_a + (0..9).to_a).sample($length).join('')"
end

function _scoiatael_maybe_load
    set -l filename $argv[-1]
    set -e argv[-1]
    if test -f $filename
        if count $argv
            eval $argv source $filename
        else
            source $filename
        end
    end
end

function _scoiatael_fish_init
    set -U fish_greeting ""
    set -U fish_key_bindings fish_vi_key_bindings
    function fish_mode_prompt
    end

    for file in ~/.config/fish/conf.d/*.fish
        source $file
    end

    which nvim;
    and alias vim nvim
    alias vi=vim
    alias pacman=yaourt
    alias em='emacsclient -nw -s console-edit -a \'\''
    alias emg='open -a Emacs'

    python -m virtualfish auto_activation compat_aliases | source

    _scoiatael_maybe_load ~/.iterm2_shell_integration.fish

    _scoiatael_maybe_load bass ~/.cargo/env
    _scoiatael_maybe_load bass ~/.rvm/scripts/rvm

    set EDITOR em

    set -l additional_user_paths /usr/local/sbin $HOME/.local/bin $HOME/Library/Python/2.7/bin /usr/local/opt/go/libexec/bin

    for p in $additional_user_paths
        if test -d $p
            set -g fish_user_paths $p $fish_user_paths
        end
    end

    which envoy
    and envoy -p | source
    ssh-add -l | grep -v 'no identities'
    or ssh-add -A ^ /dev/null
    or ssh-add
    which thefuck
    and thefuck --alias | source >/dev/null
end

_scoiatael_fish_init >/dev/null
