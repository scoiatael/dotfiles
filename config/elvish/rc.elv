use str

var extra_paths = (str:join ":" [
  $E:HOME/.nix-profile/bin
  $E:GOPATH/bin
  /usr/local/MacGPG2/bin
  $E:HOME/.emacs.d/bin
  $E:HOME/.emacs.doom/bin
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

set-env LC_ALL en_GB.UTF-8
set-env LANG en_GB.UTF-8
set-env EDITOR "emacsclient -t"
set-env GIT_EDITOR $E:EDITOR
set-env SAM_CLI_TELEMETRY 0
set-env CLOUDSDK_PYTHON python
set-env GOPATH $E:HOME/go
set-env DOOMLOCALDIR $E:HOME/.emacs.local
set-env LSP_USE_PLISTS true

if (and (!=s vscode $E:TERM_PROGRAM) ?(test -z $E:TMUX)) {
  # Not inside tmux, let's amend that

  set-env TMUX_COLORTAG_TAG_ONLY yes
  set-env TMUX_COLORTAG_USE_POWERLINE yes
  set-env TMUX_COLORTAG_ROUNDED_POWERLINE yes
  tmux-start
}

if (==s Linux (uname)) {

  set-env SSH_AUTH_SOCK (systemctl --user status gpg-agent-ssh.socket | grep Listen: | awk '{ print $2 }')
  set-env AWS_VAULT_BACKEND kwallet # nixOS workaround for https://github.com/99designs/aws-vault/issues/670
} else {
  if (or (==s $E:SSH_AUTH_SOCK "") (str:has-prefix $E:SSH_AUTH_SOCK "/private/tmp/com.apple.launchd")) {
    gpgconf --launch gpg-agent
    set-env SSH_AUTH_SOCK $E:HOME"/.gnupg/S.gpg-agent.ssh"
  }
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
if (and (has-external vagrant ) (not (has-external v))) {
  alias:new v vagrant
}
alias:new do doas -u (id -nu)-docker
alias:new mux tmuxinator
alias:new nix-do doas bash -c 'cd /etc/nixos; bash'
alias:new nix-test nix-build --keep-failed --expr 'with import <nixpkgs> {}; callPackage ./default.nix {}'
alias:new hmr home-manager switch --flake 'path:'$E:HOME'/dotfiles#framework'

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
git-completions:init

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

use github.com/zzamboni/elvish-completions/comp

set edit:completion:arg-completer[cd] = (comp:sequence [ {|stem|
      comp:files $stem &dirs-only
}])

if (has-external command-not-found) {
  set edit:after-command = (assoc $edit:after-command 0 { |args|
      var error = $args[error]
      var src = $args[src]
      if (not (is $error $nil)) {
        # NOTE: this might change soon in new Elvish: https://elv.sh/ref/language.html#exception
        if (str:has-prefix (repr $error[reason]) "<unknown exec:") {
          try {
            command-not-found (put $src[code] | str:split ' ' (one) | take 1 | str:trim (one) "\n")
          } catch {
            # on nixOS it always return 127 for some reason
          }
        }
      }
  })
}
