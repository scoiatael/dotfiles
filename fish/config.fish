function random
    set -l length $argv[1]
    if test -z $length
        set -l length 10
    end
    ruby -e "puts (('a'..'z').to_a + (0..9).to_a).sample($length).join('')"
end

function _scoiatael_fish_init
    set -U fish_greeting ""
    set -U fish_key_bindings fish_vi_key_bindings
    function fish_mode_prompt
    end

    for file in ~/.config/fish/conf.d/*.fish
        source $file
    end

    set -U LESS '-R'
    export LESS
    set -U LESSOPEN '|lesspipe.sh %s'
    export LESSOPEN

    which direnv
    and eval (direnv hook fish)

    alias b bundle
end

_scoiatael_fish_init >/dev/null

if test -f $HOME/.iterm2_shell_integration.fish;
  source $HOME/.iterm2_shell_integration.fish
end
