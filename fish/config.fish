set -U fish_key_bindings fish_vi_key_bindings

for file in ~/.config/fish/conf.d/*.fish
    source $file
end

alias vi=vim
alias pacman=yaourt
eval (python -m virtualfish auto_activation compat_aliases)

set -l iterm_integration $HOME/.iterm2_shell_integration.fish
if test -f $iterm_integration
  source $iterm_integration
end

function gitpr
  git branch --merged master | grep -v 'master$' | xargs git branch -d
end

set PATH $PATH /usr/local/opt/go/libexec/bin
