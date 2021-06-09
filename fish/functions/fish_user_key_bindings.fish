function fish_user_key_bindings
    bind -M insert \cs forward-char
    bind -M insert \cp 'prevd > /dev/null; commandline -f repaint'
    bind -M insert \cn 'nextd > /dev/null; commandline -f repaint'

    bind \cz __fzf_z
end
