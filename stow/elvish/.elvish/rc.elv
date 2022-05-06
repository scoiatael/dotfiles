use str

var extra_paths = (str:join ":" [
  $E:HOME/.nix-profile/bin
  $E:GOPATH/bin
  /usr/local/MacGPG2/bin
  $E:HOME/.emacs.d/bin
  $E:HOME/.local/bin
  $E:HOME/dotfiles/bin
  $E:HOME/.cargo/bin
  /usr/local/opt/sqlite/bin
  /usr/local/Caskroom/google-cloud-sdk/latest/google-cloud-sdk/bin
  /usr/local/bin
  $E:PATH
])

set-env PATH $extra_paths
fn tmux-start {
  if ?(e:tmux ls > /dev/null 2> /dev/null) {
    exec tmux attach
  } else {
    exec tmux
  }
}

var ssh_agent = $E:HOME"/.gnupg/S.gpg-agent.ssh"
if (and (!=s vscode $E:TERM_PROGRAM) ?(test -z $E:TMUX)) {
  # Not inside tmux, let's amend that
  set-env LC_ALL en_GB.UTF-8
  set-env LANG en_GB.UTF-8
  set-env EDITOR "emacsclient -c"
  set-env GIT_EDITOR $E:EDITOR
  set-env SAM_CLI_TELEMETRY 0
  set-env CLOUDSDK_PYTHON python
  set-env GOPATH $E:HOME/go


  set-env TMUX_COLORTAG_TAG_ONLY yes
  set-env TMUX_COLORTAG_USE_POWERLINE yes
  set-env TMUX_COLORTAG_ROUNDED_POWERLINE yes
  tmux-start
}

fn __launch_gpg_agent {
  gpgconf --launch gpg-agent
}

if (==s Linux (uname)) {
  set __launch_gpg_agent~ = {
    systemctl --user enable --now 'gpg-agent.socket'
  }

  set-env AWS_VAULT_BACKEND kwallet # nixOS workaround for https://github.com/99designs/aws-vault/issues/670
}

if (==s E:SSH_AUTH_SOCK "") {
  __launch_gpg_agent
  set-env SSH_AUTH_SOCK $ssh_agent
}

use direnv
use zoxide

# NOTE: epm:install &silent-if-installed=true _still_ does connection over internet. Which means no net = no shell. Bad idea.
fn _lazy_install { |@pkgs|
  use epm
  peach { |pkg| if (not (epm:is-installed $pkg)) { epm:install $pkg } } $@pkgs
}

_lazy_install [
  github.com/zzamboni/elvish-themes
  github.com/zzamboni/elvish-modules
  github.com/zzamboni/elvish-completions
  github.com/xiaq/edit.elv
]

use github.com/zzamboni/elvish-modules/bang-bang

use github.com/zzamboni/elvish-modules/util

use starship

fn ls {|@a| e:ls $@a }
if (has-external exa) {
    # Cannot use fn here, since it'd declare function in local scope.
    set ls~ = {|@a| e:exa $@a }
}

fn mvbak {|path|
  var ts = (e:date +%Y%m%dT%H%M | str:trim (one) "\n")
  var dst = (str:join "" ["." $path '.bak.' $ts])
  var c = 0
  while ?(test -f $dst) {
    set dst = (str:join "" ["." $path '.bak.' $ts '_' (to-string $c)])
    set c = (+ $c 1)
  }
  e:mv $path $dst
}

fn unmvbak {|path|
  var dst src
  if ?(test -f $path) {
    set src = $path
    set dst = [(str:split "." $path)][1]
  } else {
    set dst = $path
    set src = [(echo .$path.bak.* | str:split " " (all))][0]
  }
  e:mv $src $dst
}

fn _edit_prompt {
  var tmp = (mktemp "_edit_prompt.XXXXXXXXXX.elv")
  chmod 600 $tmp
  try {
    echo $edit:current-command > $tmp
    edit $tmp
    set edit:current-command = (e:cat $tmp | str:join "\n")
  } finally {
    rm $tmp
  }
}

set edit:insert:binding[Ctrl-L] = { clear > /dev/tty; edit:redraw &full=$true }
set edit:insert:binding[Ctrl-E] = { edit:move-dot-eol }
set edit:insert:binding[Ctrl-A] = { edit:move-dot-sol }
set edit:insert:binding[Ctrl-X] = { _edit_prompt }
set edit:insert:binding[Alt-m] = $edit:-instant:start~
set edit:insert:binding[Alt-d] = $edit:kill-small-word-right~

set edit:prompt-stale-transform = {|x| styled $x "bright-black" }

use github.com/zzamboni/elvish-completions/comp

var bolt-task-completions = [
  &run= (comp:sequence &opts={ bolt task run -h | comp:extract-opts } [ { jq -r 'keys | join("\n")' .task_cache.json } ] )
  &show= (comp:sequence &opts={ bolt task show -h | comp:extract-opts } [ { jq -r 'keys | join("\n")' .task_cache.json } ] )
]

var bolt-plan-completions = [
  &run= (comp:sequence &opts={ bolt plan run -h | comp:extract-opts } [ { jq -r 'keys | join("\n")' .plan_cache.json } ] )
  &show= (comp:sequence &opts={ bolt plan show -h | comp:extract-opts } [ { jq -r 'keys | join("\n")' .plan_cache.json } ] )
]

var bolt-completions = [
  &task= (comp:subcommands $bolt-task-completions)
  &plan= (comp:subcommands $bolt-plan-completions) ]

set edit:completion:arg-completer[bolt] = (comp:subcommands $bolt-completions)

use github.com/zzamboni/elvish-modules/alias

alias:new dfc e:dfc -p -/dev/disk1s4,devfs,map,com.apple.TimeMachine
alias:new cat bat
alias:new more bat --paging always
alias:new v vagrant
alias:new do doas -u (id -nu)-docker

fn aws-do {|creds @rest|
  aws-vault exec $creds -- doas -u (id -nu)-docker $@rest
}

fn aws-prepare {
  mkdir -p .aws-sam/build
  chmod g+w .aws-sam
  chmod g+w .aws-sam/build

  touch .aws-sam/build.toml
  chmod g+w .aws-sam/build.toml

  touch samconfig.toml
  chmod g+w samconfig.toml
}

set E:MANPAGER = "sh -c 'col -bx | bat -l man -p'"

use github.com/xiaq/edit.elv/smart-matcher
smart-matcher:apply

use github.com/zzamboni/elvish-completions/git git-completions

use github.com/zzamboni/elvish-modules/long-running-notifications
# remember to `brew install terminal-notifier`
set long-running-notifications:threshold = 30
set long-running-notifications:never-notify = [ vi vim emacs nano less more bat cat ssh python ipython irb pry flask lima ]

use github.com/zzamboni/elvish-modules/util-edit
util-edit:electric-delimiters

use github.com/zzamboni/elvish-modules/dir
set edit:insert:binding[Alt-b] = $dir:left-word-or-prev-dir~
set edit:insert:binding[Alt-f] = $dir:right-word-or-next-dir~
set edit:insert:binding[Alt-i] = $dir:history-chooser~
fn cd { |@a| dir:cd $@a }

