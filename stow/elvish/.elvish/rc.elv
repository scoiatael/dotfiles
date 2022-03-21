fn tmux-start {
  if ?(test -z $E:TMUX) {
    # Not inside tmux, let's amend that
    if ?(tmux ls > /dev/null 2> /dev/null) {
      exec asdf exec direnv exec ~ tmux attach
    } else {
      exec asdf exec direnv exec ~ tmux
    }
  }
}

# https://asdf-vm.com/guide/getting-started.html#_3-install-asdf
if ?(test -d /opt/asdf-vm) { # Arch
  set-env ASDF_DIR /opt/asdf-vm
} elif ?(test -d  /usr/local/opt/asdf/libexec ) {
  set-env ASDF_DIR /usr/local/opt/asdf/libexec
} elif ?(test -d /run/current-system/sw/share/asdf-vm/) { # nixOS
  set-env ASDF_DIR /run/current-system/sw/share/asdf-vm
}
set-env ASDF_DATA_DIR $E:HOME'/.asdf'
use asdf _asdf; var asdf~ = $_asdf:asdf~
set edit:completion:arg-completer[asdf] = $_asdf:arg-completer~

use direnv
use zoxide

if (!=s vscode $E:TERM_PROGRAM) {
  set-env TMUX_COLORTAG_TAG_ONLY yes
  set-env TMUX_COLORTAG_USE_POWERLINE yes
  set-env TMUX_COLORTAG_ROUNDED_POWERLINE yes
  tmux-start
}

use epm
epm:install github.com/zzamboni/elvish-themes&silent-if-installed=$true
epm:install github.com/zzamboni/elvish-modules&silent-if-installed=$true
epm:install github.com/zzamboni/elvish-completions&silent-if-installed=$true
epm:install github.com/xiaq/edit.elv&silent-if-installed=$true

use github.com/zzamboni/elvish-modules/bang-bang

use github.com/zzamboni/elvish-modules/util

use github.com/zzamboni/elvish-themes/chain
set chain:prompt-segments = [ su git-combined arrow ]
set chain:rprompt-segments = [ dir ]
set chain:glyph[arrow] = "λ"
set chain:glyph[chain] = " "
chain:init

use str

fn ls {|@a| e:ls -G $@a }
if (which exa) {
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

set edit:insert:binding[Ctrl-L] = { clear > /dev/tty; edit:redraw &full=$true }
set edit:insert:binding[Ctrl-E] = { edit:move-dot-eol }
set edit:insert:binding[Ctrl-A] = { edit:move-dot-sol }
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