#!/usr/bin/env fish

function __fzf_z --description 'Quick cd with z and fzf'
    # Make sure that fzf uses fish so we can run fish_indent.
    # See similar comment in __fzf_search_shell_variables.fish.
    set --local --export SHELL (command --search fish)

    set project (
        z --list |
        awk '{ print $2 }' |
        fzf \
            --tiebreak=index \
            --query=(commandline) \
            # preview current command using fish_ident in a window at the bottom 3 lines tall
            --preview="echo -- {4..} | fish_indent --ansi" \
            --preview-window="bottom:3:wrap" |
        string collect
    )

    if test $status -eq 0
        cd $project
    end

    commandline --function repaint
end
