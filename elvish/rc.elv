if (not ?(test -f ~/.elvish/lib/direnv.elv)) {
  direnv hook elvish > ~/.elvish/lib/direnv.elv
}
use direnv

eval (zoxide init elvish | slurp)

use epm
epm:install github.com/zzamboni/elvish-themes&silent-if-installed=$true
epm:install github.com/zzamboni/elvish-modules&silent-if-installed=$true
epm:install github.com/zzamboni/elvish-completions&silent-if-installed=$true

use github.com/zzamboni/elvish-modules/bang-bang

use github.com/zzamboni/elvish-themes/chain
chain:prompt-segments = [ su git-combined arrow ]
chain:rprompt-segments = [ dir ]
chain:glyph[arrow] = "λ"
chain:glyph[chain] = " "
chain:init

fn ls [@a]{ e:ls -G $@a }
if (which exa) {
    # Cannot use fn here, since it'd declare function in local scope.
    ls~ = [@a]{ e:exa $@a }
}

edit:insert:binding[Ctrl-L] = { clear > /dev/tty; edit:redraw &full=$true }
edit:insert:binding[Ctrl-E] = { edit:move-dot-eol }
edit:insert:binding[Ctrl-A] = { edit:move-dot-sol }

use github.com/zzamboni/elvish-completions/comp

bolt-task-completions = [
  &run= (comp:sequence &opts={ bolt task run -h | comp:extract-opts } [ { jq -r 'keys | join("\n")' .task_cache.json } ] )
  &show= (comp:sequence &opts={ bolt task show -h | comp:extract-opts } [ { jq -r 'keys | join("\n")' .task_cache.json } ] )
]

bolt-plan-completions = [
  &run= (comp:sequence &opts={ bolt plan run -h | comp:extract-opts } [ { jq -r 'keys | join("\n")' .plan_cache.json } ] )
  &show= (comp:sequence &opts={ bolt plan show -h | comp:extract-opts } [ { jq -r 'keys | join("\n")' .plan_cache.json } ] )
]

bolt-completions = [
  &task= (comp:subcommands $bolt-task-completions)
  &plan= (comp:subcommands $bolt-plan-completions) ]

edit:completion:arg-completer[bolt] = (comp:subcommands $bolt-completions)

use str
