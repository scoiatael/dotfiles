if (not (test -f $HOME/.elvish/lib/direnv.elv)) {
  direnv hook elvish > $HOME/.elvish/lib/direnv.elv
}
use direnv

eval (zoxide init elvish | slurp)

use epm
epm:install github.com/zzamboni/elvish-themes&silent-if-installed=$true

use github.com/zzamboni/elvish-themes/chain
chain:init

fn ls [@a]{ e:exa $@a }

edit:insert:binding[Ctrl-L] = { clear > /dev/tty; edit:redraw &full=$true }
edit:insert:binding[Ctrl-E] = { edit:move-dot-eol }
edit:insert:binding[Ctrl-A] = { edit:move-dot-sol }
