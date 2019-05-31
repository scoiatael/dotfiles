set -l dotfiles_path "$HOME/dotfiles/fish"
set fish_function_path $dotfiles_path/functions $fish_function_path

fundle plugin 'edc/bass'
fundle plugin 'laughedelic/pisces'
fundle plugin 'oh-my-fish/plugin-bak'
fundle plugin 'oh-my-fish/plugin-bundler'
fundle plugin 'oh-my-fish/plugin-wttr'
fundle plugin 'jethrokuan/z'
fundle plugin 'jethrokuan/fzf'

fundle init

set -q FZF_TMUX_HEIGHT; or set -U FZF_TMUX_HEIGHT "40%"
set -q FZF_DEFAULT_OPTS; or set -U FZF_DEFAULT_OPTS "--height $FZF_TMUX_HEIGHT"
set -q FZF_LEGACY_KEYBINDINGS; or set -U FZF_LEGACY_KEYBINDINGS 1
set -q FZF_PREVIEW_FILE_CMD; or set -U FZF_PREVIEW_FILE_CMD "head -n 10"
set -q FZF_PREVIEW_DIR_CMD; or set -U FZF_PREVIEW_DIR_CMD "ls"


function random
    set -q argv[1]; and set -l length $argv[1]; or set -l length 32
    cat /dev/urandom | gtr -dc 'a-zA-Z0-9' | fold -w $length | head -n 1
end

function reset_gpg_agent
  gpgconf --kill gpg-agent
  gpgconf --launch gpg-agent
end

function install_node
  set -l node_version $argv[1]
  if test -z $node_version
      set -l node_version (curl -s http://nodejs.org/dist/index.tab | head -n 2  | tail -n 1 | awk '{ print $1 }')
  end
  set -l install_dir $NODE_VERSIONS/node-$node_version
  set -l node_url http://nodejs.org/dist/$node_version/node-$node_version-darwin-x64.tar.gz
  echo "Installing node $node_version to $install_dir"
  echo "   from $node_url"
  mkdir -p $install_dir
  curl -fsSL "$node_url" | tar xvz --strip 1 -C $install_dir
end

function _scoiatael_fish_init
    set -U fish_greeting ""
    set -U fish_key_bindings fish_hybrid_key_bindings
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
