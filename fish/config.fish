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

function _scoiatael_launch_gpg_agent
  gpgconf --launch gpg-agent

  set -U SSH_AGENT_SOCK (gpgconf --list-dirs agent-ssh-socket)
  export SSH_AGENT_SOCK
end

function _scoiatael_fish_init
    set -U fish_greeting ""
    set -U fish_key_bindings fish_vi_key_bindings
    function fish_mode_prompt
    end

    for file in ~/.config/fish/conf.d/*.fish
        source $file
    end

    python -m virtualfish auto_activation compat_aliases | source

    set -q SSH_AGENT_SOCK
    or _scoiatael_launch_gpg_agent

    which direnv
    and eval (direnv hook fish)
end

_scoiatael_fish_init >/dev/null
