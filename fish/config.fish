set -l dotfiles_path "$HOME/dotfiles/fish"
set fish_function_path $dotfiles_path/fisherman $fish_function_path
set fish_function_path $dotfiles_path/functions $fish_function_path

function random
    set -l length $argv[1]
    if test -z $length
        set -l length 10
    end
    ruby -e "puts (('a'..'z').to_a + (0..9).to_a).sample($length).join('')"
end

function install_node
  set -l version $argv[1]
  if test -z $version
      set -l version (curl -s http://nodejs.org/dist/index.tab | head -n 2  | tail -n 1 | awk '{ print $1 }')
  end
  set -l install_dir $NODE_VERSIONS/node-$version
  echo "Installing node $version to $install_dir"
  mkdir -p $install_dir
  curl -fsSL http://nodejs.org/dist/$version/node-$version-darwin-x64.tar.gz | tar xvz --strip 1 -C $install_dir
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
    alias gls "git status"
    alias g git
end

_scoiatael_fish_init >/dev/null

if test -f $HOME/.iterm2_shell_integration.fish;
  source $HOME/.iterm2_shell_integration.fish
end
