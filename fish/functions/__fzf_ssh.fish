#!/usr/bin/env fish

function __fzf_ssh --description 'Quick ssh with fzf'
    # Make sure that fzf uses fish so we can run fish_indent.
    # See similar comment in __fzf_search_shell_variables.fish.
    set --local --export SHELL (command --search fish)

    set host (
        cat ~/.ssh/known_hosts |
        awk '{ split($1, A, ","); print(A[1]) }' |
        fzf \
            --tiebreak=index \
            --query=(commandline) \
            # preview current command using fish_ident in a window at the bottom 3 lines tall
            --preview="echo -- {4..} | fish_indent --ansi" \
            --preview-window="bottom:3:wrap" |
        string collect
    )

    if test $status -eq 0
        direnv export fish | source
        clear
        exec ssh $host
    end

    commandline --function repaint
end
