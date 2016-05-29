set -U fish_key_bindings fish_vi_key_bindings

for file in ~/.config/fish/conf.d/*.fish
    source $file
end

alias vi=vim
alias pacman=yaourt
