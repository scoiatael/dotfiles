set -l dotfiles_path "$HOME/dotfiles/fish"
set fish_function_path $dotfiles_path/functions $fish_function_path

fundle plugin 'edc/bass'
fundle plugin 'laughedelic/pisces'
fundle plugin 'oh-my-fish/plugin-bak'
fundle plugin 'oh-my-fish/plugin-wttr'
fundle plugin 'jethrokuan/z'
fundle plugin 'jethrokuan/fzf'
fundle plugin 'aliz-ai/google-cloud-sdk-fish-completion'

fundle init

set -q FZF_TMUX_HEIGHT; or set -U FZF_TMUX_HEIGHT "40%"
set -q FZF_DEFAULT_OPTS; or set -U FZF_DEFAULT_OPTS "--height $FZF_TMUX_HEIGHT"
set -q FZF_LEGACY_KEYBINDINGS; or set -U FZF_LEGACY_KEYBINDINGS 1
set -q FZF_PREVIEW_FILE_CMD; or set -U FZF_PREVIEW_FILE_CMD "head -n 10"
set -q FZF_PREVIEW_DIR_CMD; or set -U FZF_PREVIEW_DIR_CMD "ls"

function reset_gpg_agent
  gpgconf --kill gpg-agent
  gpgconf --launch gpg-agent
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


    set -l iterm_integration_fish $HOME/.iterm2_shell_integration.fish
    if test -f $iterm_integration_fish;
      source $iterm_integration_fish
    end

    set -l gcloud_path_fish /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/path.fish.inc
    if test -f $gcloud_path_fish;
      source $gcloud_path_fish
    end
end

_scoiatael_fish_init >/dev/null
